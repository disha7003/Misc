library("recommenderlab")

m <- matrix(sample(c(as.numeric(0:5), NA), 50, replace=TRUE, prob=c(rep(.4/6,6),.6)), ncol=10, dimnames=list(user=paste("u", 1:5, sep=''),item=paste("i", 1:10, sep=''))
)
m
r <- as(m, "realRatingMatrix")
r

#as(r,"dgCMatrix")

identical(as(r, "matrix"),m)
as(r, "list")
head(as(r, "data.frame"))
r_m <- normalize(r)
r_m
image(r, main = "Raw Ratings")
image(r_m, main = "Normalized Ratings")

r_b <- binarize(r, minRating=1)
r_b
as(r_b, "matrix")

#################

data("Jester5k")
Jester5k

r <- sample(Jester5k, 1000)
r
#first user ratings
rowCounts(r[1,])
as(r[1,], "list")
rowMeans(r[1,])
hist(getRatings(r), breaks=100)
hist(getRatings(normalize(r)), breaks=100)
hist(getRatings(normalize(r, method="Z-score")), breaks=100)


hist(rowCounts(r), breaks=50) #how many jokes each user has rated
hist(colMeans(r), breaks=20) #mean rating for each Joke

recommenderRegistry$get_entries(dataType = "realRatingMatrix")
r <- Recommender(Jester5k[1:1000], method = "POPULAR")
r
names(getModel(r))
getModel(r)$topN

recom <- predict(r, Jester5k[1001:1002], n=5)
recom
as(recom, "list")

recom3 <- bestN(recom, n = 3)
recom3
as(recom3, "list")

recom <- predict(r, Jester5k[1001:1002], type="ratings")
recom
as(recom, "matrix")[,1:10]

#evaluation
e <- evaluationScheme(Jester5k[1:1000], method="split", train=0.9,given=15, goodRating=5)
e

r1 <- Recommender(getData(e, "train"), "UBCF")
r2 <- Recommender(getData(e, "train"), "IBCF")
r1
r2

p1 <- predict(r1, getData(e, "known"), type="ratings")
p2 <- predict(r2, getData(e, "known"), type="ratings")
p1
p2

error <- rbind(calcPredictionAccuracy(p1, getData(e, "unknown")),calcPredictionAccuracy(p2, getData(e, "unknown")))
rownames(error) <- c("UBCF","IBCF")
error

#evaluation of topN
scheme <- evaluationScheme(Jester5k[1:1000], method="cross", k=4, given=3,goodRating=5)
scheme
results <- evaluate(scheme, method="POPULAR", type = "topNList",n=c(1,3,5,10,15,20))
results
getConfusionMatrix(results)[[1]]
avg(results)
plot(results,annotate=TRUE)
plot(results, "prec/rec", annotate=TRUE)

#Comparing top-N recommendations
set.seed(2016)
scheme <- evaluationScheme(Jester5k[1:1000], method="split", train = .9,k=1, given=-5, goodRating=5)
scheme

algorithms <- list(   "random items" = list(name="RANDOM", param=NULL),   "popular items" = list(name="POPULAR", param=NULL),   "user-based CF" = list(name="UBCF", param=list(nn=50)),   "item-based CF" = list(name="IBCF", param=list(k=50)),   "SVD approximation" = list(name="SVD", param=list(approxRank = 50))   )

results <- evaluate(scheme, algorithms, type = "topNList", n=c(1, 3, 5, 10, 15, 20))
results
names(results)
results[["user-based CF"]]
plot(results, annotate=c(1,3), legend="bottomright")
plot(results, "prec/rec", annotate=3, legend="topleft")

#Comparing ratings
results <- evaluate(scheme, algorithms, type = "ratings")
results
plot(results, ylim = c(0,100))

#Using a 0-1 data set
Jester_binary <- binarize(Jester5k, minRating=5)
Jester_binary <- Jester_binary[rowCounts(Jester_binary)>20]
Jester_binary
scheme_binary <- evaluationScheme(Jester_binary[1:1000],method="split", train=.9, k=1, given=3)
scheme_binary

results_binary <- evaluate(scheme_binary, algorithms,type = "topNList", n=c(1,3,5,10,15,20))
plot(results_binary, annotate=c(1,3), legend="bottomright")


###########
# # ## always recommends the top - N popular items (without known items)
REAL_POPULAR <-
  function(data, parameter = NULL) {
    p <-
      .get_parameters(list(
        normalize = "center",
        aggregation = colSums ## could also be colMeans
      ), parameter)
    
    ## normalize data
    if (!is.null(p$normalize))
      data <-
        normalize(data, method = p$normalize)
    
    topN <-
      new(
        "topNList",
        items = list(order(p$aggregation(data), decreasing =
                             TRUE)),
        itemLabels = colnames(data),
        n = ncol(data)
      )
    
    ratings <-
      new("realRatingMatrix", data = dropNA(t(colMeans(data))))
    model <-
      c(list(topN = topN, ratings = ratings), p)
    predict <-
      function(model, newdata, n = 10, type = c("topNList", "ratings"), ...) {
        type <-
          match.arg(type)
        if (type == "topNList") {
          topN <-
            removeKnownItems(model$topN, newdata, replicate = TRUE)
          topN <-
            bestN(topN, n)
          return(topN)
        }
        
        ## type == "ratings"
        if (!is.null(model$normalize))
          
          newdata <-
            normalize(newdata, method = model$normalize)
        ratings <-
          removeKnownRatings(model$ratings, newdata, replicate = TRUE)
        ratings <-
          denormalize(ratings, factors = getNormalize(newdata))
        return(ratings)
      }
    ## construct and return the recommender object
    new(
      "Recommender", method = "POPULAR", dataType = class(data),
      ntrain = nrow(data), model = model, predict = predict
    )
  }
## register recommender
recommenderRegistry$set_entry(
  method = "POPULAR", dataType = "realRatingMatrix", fun = REAL_POPULAR,
  description =
    "Recommender based on item popularity (real data)."
)

