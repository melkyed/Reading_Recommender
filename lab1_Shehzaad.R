#### Importing Libraries 
library(tidyverse)
library(recommenderlab)
library(reshape2)
library(data.table)


#### Reading Datasets with read.csv
books <- read.csv("books.csv", encoding = "ISO-8859-1", stringsAsFactors = FALSE)
ratings <- read.csv("ratings.csv", encoding = "ISO-8859-1")
book_tags <- read.csv("book_tags.csv", encoding = "ISO-8859-1")
tags <- read.csv("tagsb.csv", encoding = "ISO-8859-1", stringsAsFactors = FALSE)

#### Exploratory analysis and cleaning----
str(books) 
#lots of good information about the books here, it has a ratings count as well which might come in handy
#datatypes look good the strings were automatically read as factors so changed the read.csv of Books

str(ratings)
#ratings of each book by user here 
#books and ratings can be joined by book_id if need be
#close to a million ratings of the 10000 books!

str(book_tags)
#looks like each book has a tagID that users assigned to the books

str(tags)
#looks like the tag names attached to each tagID, on closer inspection it looks like an inconsistent mix,
#of genres and descriptions 

#### Basic statistics 
summary(books)
#ISBN looks like it needs to be changed from char to num
books1 <- as.numeric(books$isbn)
summary(books1)
books$isbn <- as.numeric(books$isbn)
summary(books)
#isbn converted to numeric
#some columns have missing values(isbn,original publication year) we will see if these will need to be dealt with 
#further exploration neccesary for feature importance and outliers

summary(ratings)
summary(book_tags)
summary(tags)
#nothing out of the ordinary here

#Exploring for duplicate values

sum(duplicated(books))
#no duplicated rows!

sum(duplicated(ratings))
sum(duplicated(ratings[,1:2]))
#1644 duplicated rows here 
#2278 duplicates when consdering just book_id and user_id columns 

sum(duplicated(book_tags))
sum(duplicated(book_tags[,1:2]))
#6 duplicated rows here
#8 when considering  just goodreads_book_id and tag_id

sum(duplicated(tags))
#no duplicated rows 

#### Removing duplicates dplyr
ratings1 <- ratings %>% 
  distinct(book_id, user_id, .keep_all = TRUE)
dim(ratings1)
sum(duplicated(ratings1)) #Duplicated rows removed

book_tags1 <- book_tags %>%
  distinct(goodreads_book_id, tag_id, .keep_all = TRUE)
dim(book_tags1)
sum(duplicated(book_tags1))#Duplicated rows removed

head(ratings1)
head(book_tags1)

#### Further exploration and Data Visualizations ----

top_ratings <- books %>%
  arrange(desc(average_rating))

head(select(top_ratings, title, average_rating), 25) #top25 books by rating average, Harry potter and Calvin and Hobbs dominating 
head(select(top_ratings, authors), 25) 

rating_count <- books %>%
  arrange(desc(ratings_count))

head(select(rating_count, title, ratings_count), 25) #books that were rated the most

gg_rating_count <- head(select(rating_count, title, ratings_count), 25)

books$language_code <- as.factor(books$language_code)
lang_code <- books %>% 
  group_by(language_code) %>%
  tally(sort = TRUE)#mostly books in english rated here

low_ratings <- books %>% 
  arrange(average_rating)

head(select(low_ratings, title, average_rating),25)#lowest rated books

user_count <- ratings1 %>% #the users who rated the most books shown here
  group_by(user_id) %>% 
  count(user_id) %>% 
  arrange(desc(n))

head(user_count, 25)

user_count_less <- ratings1 %>% #the users who rated the least books shown here
  group_by(user_id) %>% 
  count(user_id) %>% 
  arrange((n))

head(user_count_less, 25)

pop_ratings <- ratings1 %>% 
  group_by(rating) %>% 
  tally(sort = TRUE) #most popular rating given to our sample books lots of 4s and 5s!


tags_bind <- merge(book_tags1, tags, by = "tag_id") #descriptions and genres so mixed, need to organise them
tags_bind$tag_name <- as.factor(tags_bind$tag_name) #change to factor so we can filter out the levels

descr_count <- tags_bind %>% group_by(tag_name) %>% 
  tally(sort = TRUE)

selected <- str_to_lower(c("Art", "Biography", "Business", "Chick Lit", "Children's", "Christian", "Classics", "Comics",
                           "Contemporary", "Cookbooks", "Crime", "Ebooks", "Fantasy", "Fiction", "Gay and Lesbian", 
                           "Graphic Novels", "Historical Fiction", "History", "Horror", "Humor and Comedy", "Manga", 
                           "Memoir", "Music", "Mystery", "Nonfiction", "Paranormal", "Philosophy", "Poetry", 
                           "Psychology", "Religion", "Romance", "Science", "Science Fiction", "Self Help", 
                           "Suspense", "Spirituality", "Sports", "Thriller", "Travel", "Young Adult"))

pop_genre <- descr_count[descr_count$tag_name %in% selected, ]


#### Plots 

ggplot(data = gg_rating_count,
       mapping = aes(x = reorder(title, ratings_count),
                     y = ratings_count, fill = reorder(title, ratings_count))) +
  geom_bar(stat = "identity") + coord_flip()+
  guides (fill = FALSE) +
  labs(x = "Title", y = "Ratings Count") # Books that were rated the most


ggplot(data = lang_code,
       mapping = aes(x = reorder(language_code, n),
                     y = n, fill = "red")) +
  geom_bar(stat = "identity") + 
  scale_y_log10() +
  coord_flip() + 
  labs(x = "Languages", y = "Language Count")#mostly english books rated

ggplot(data = pop_ratings,
       mapping = aes(x = rating,
                     y = n, fill = reorder(rating, n))) +
  geom_bar(stat = "identity")+
  guides (fill = FALSE) + 
  labs(x = "Rating", y = "Ratings Count") # 4 being the most popular rating


ggplot(data = pop_genre,
       mapping = aes(x = reorder(tag_name, n),
                     y = n, fill = n)) +
  geom_bar(stat = "identity") + coord_flip()+
  guides (fill = FALSE) + 
  labs(x = "Genres", y = "Count of Genres")#genres and descriptions that were rated the most


##### Data Preparation for Modelling ----

summary(user_count)
set.seed(1234)
sample_tr <- sample_n(ratings1, 15000) #took a sample of the data to create the matrix as the entire dataset is too big(not good for traini)

ratings2 <- ratings1 %>% # better sample of with users who rated more than 180 books, more dense
  group_by(user_id) %>% 
  filter(n()> 180)


bookMatrix <- acast(ratings2, user_id ~ book_id) #using acast to create a matrix like stucture 
class(bookMatrix)
dim(bookMatrix)

matrix_a <- as.matrix(bookMatrix)
matrix_rr <- as(matrix_a, "realRatingMatrix") #converting to realRatingMatrix data structure
image(matrix_rr, useRaster = TRUE)
image(matrix_rr)
matrix_a[1:10, 1:10]
matrix_rr


matrix_b <- matrix_a
matrix_b[is.na(matrix_b)] <- 0
sparse_ratings <- as(matrix_b, "sparseMatrix") #using this method to save storage space 
sparse_matrix_rr <- as(sparse_ratings, "realRatingMatrix")
dim(sparse_matrix_rr)
image(sparse_matrix_rr)
sparse_matrix_rr #this sample dataset is more dense and will be better to model

sp_mat_NA <- as(dropNA(matrix_b), "sparseMatrix")
smrr <- as(sp_mat_NA,"realRatingMatrix") #this matrix provided best results after trying above options


#### Model Training
recrm<- sparse_matrix_rr[,colCounts(sparse_matrix_rr) >5] #using a small dataset to train with
recrm
image(recrm)

recrma <-smrr[,colCounts(smrr) >5]
recrma
image(recrma)

#alternate option to evaluate below##
#create a 90/10 split to train the data for the first
scheme <- evaluationScheme(recrm, method = "split", train = .8,
                           k = 1, given = 10, goodRating = 3)
scheme
scheme1 <- evaluationScheme(recrma, method = "split", train = .8,
                            k = 1, given = 10, goodRating = 3) #Ran this option 


rectr <- getData(scheme1, "train")
rectr

rec_known <- getData(scheme1, "known")
rec_known

rec_unknown <- getData(scheme1, "unknown")
rec_unknown

algorithms <- list(
  "random items" = list(name="RANDOM", param=list(normalize = "Z-score")),
  "popular items" = list(name="POPULAR", param=list(normalize = "Z-score")),
  "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score", method="Jaccard", nn=3)),
  "item-based CF" = list(name="IBCF", param=list(normalize = "Z-score", method="Cosine"))
)

#evaluate the results
eval_score <- evaluate(scheme1, algorithms, n=c(1, 3, 5, 10, 15, 20))
avg(eval_score)
plot(eval_score) #looks like popular and UBCF are slightly better than the random option

#Run some UBCF models 
rec_ubcf1 <- Recommender(rectr, "UBCF", param=list(normalize = "z-score",method="Jaccard",nn=3))
rec_ubcf2 <- Recommender(rectr, "UBCF", param=list(normalize = "z-score",method="Cosine",nn=3))
rec_ubcf3 <- Recommender(rectr, "UBCF", param=list(normalize = "z-score",method="Cosine", nn=5))
rec_ubcf4 <- Recommender(rectr, "UBCF", param=list(normalize = "z-score",method="Jaccard", nn=5))
rec_ubcf5 <- Recommender(rectr, "UBCF", param=list(normalize = "z-score",method="Cosine", nn=25))
rec_ubcf6 <- Recommender(rectr, "UBCF", param=list(normalize = "z-score",method="Jaccard", nn=50))


rec_ubcf1

#make predictions

pred1 <- predict(rec_ubcf1, rec_known, type="ratings") #UBCF with Jaccard and nn = 3
pred1

pred2 <- predict(rec_ubcf2, rec_known, type="ratings") #UBCF with Cosine and nn = 3
pred2

pred3 <- predict(rec_ubcf3, rec_known, type="ratings") #UBCF with Cosine and nn = 5
pred3

pred4 <- predict(rec_ubcf4, rec_known, type="ratings") #UBCF with Jaccard and nn = 5
pred4

pred5 <- predict(rec_ubcf5, rec_known, type="ratings") #UBCF with Cosine Similarity and nn = 25
pred5

pred6 <- predict(rec_ubcf6, rec_known, type="ratings") #UBCF with Jaccard and nn = 50
pred6


#Evaluate with error metrics
pred1_acc <- calcPredictionAccuracy(pred1, rec_unknown) 
pred1_acc 

pred2_acc <- calcPredictionAccuracy(pred2, rec_unknown)
pred2_acc

pred3_acc <- calcPredictionAccuracy(pred3, rec_unknown)
pred3_acc

pred4_acc <- calcPredictionAccuracy(pred4, rec_unknown)
pred4_acc

pred5_acc <- calcPredictionAccuracy(pred5, rec_unknown)
pred5_acc

pred6_acc <- calcPredictionAccuracy(pred6, rec_unknown)#lowest RMSE
pred6_acc


#Looking at predicted values
as(rec_unknown, "matrix")[1:10,1:10] #print matrix of unknown values

as(pred6, "matrix")[1:20,1:10] #looking at predicted values, some users have no predictions at all, interesting







#### save files for shiny app#########

#### Save file for popular genre for Shiny app
saveRDS(pop_genre,file="pop_genre.RDS")

#### Save file for Top rated books list for Shiny app
saveRDS(top_ratings,file="top_ratings.RDS")

#### save files for book list for shiny app
booklist <-books %>%
  select (title, book_id)%>%
  unique()
head(booklist)

saveRDS(booklist,file="booklist.RDS")

##### Save file for popular ratings for Shiny app
saveRDS(pop_ratings,file="pop_ratings.RDS")


##### Save file for matrix for Shiny app
saveRDS(smrr,file="matrix.RDS")

##### Save file for recommender model for Shiny app
saveRDS(rec_ubcf6,file="rec_model.RDS")









