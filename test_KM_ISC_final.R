library(testthat)
import::here(priors, w, data_generating_model, distance_statistic, generate_abc_sample,
             .from = 'KYL_ISC_final.R')

context("Check final project function")

# We just use previous data to test, which can make this testing easier.

A77 <- matrix(c(66,87,25,22, 4, 
                13,14,15, 9, 4,
                NA, 4, 4, 9, 1,
                NA, NA, 4, 3, 1, 
                NA, NA, NA, 1, 1,
                NA, NA, NA, NA, 0 ), nrow = 6, ncol = 5, byrow = TRUE)

A80 <- matrix(c(44,62,47,38, 9, 
                10,13, 8,11, 5,
                NA, 9, 2, 7, 3,
                NA, NA, 3, 5, 1, 
                NA, NA, NA, 1, 0,
                NA, NA, NA, NA, 1 ), nrow = 6, ncol = 5, byrow = TRUE)

A77 <- sweep(A77, 2, colSums(A77,na.rm=T), "/")
A80 <- sweep(A80, 2, colSums(A80,na.rm=T), "/")

observed_data = list(D1 = A77,D2 = A80)

# --- tests --- #

test_that("priors output has correct length", {
  expect_equal(length(priors()), 4)
})

j=3
s=5
qc=0.5
qh=0.7

test_that("w output has correct length", {
  expect_equal(length(w(3,5,0.5,0.7)), 1)
})

test_that("data_generating_model output has correct row number", {
  expect_true(nrow(data_generating_model(5,0.5,0.7)) == 6)
})

test_that("data_generating_model output has correct col number", {
  expect_true(ncol(data_generating_model(5,0.5,0.7)) == 5)
})

qc1=0.98707071
qh1=0.35960089
qc2=0.42268729
qh2=0.01528807
D1 = observed_data$D1
D2 = observed_data$D2
Dpred1 = data_generating_model(ncol(D1),qc1,qh1)
Dpred2 = data_generating_model(ncol(D2),qc2,qh2)

test_that("distance_statistic output has correct length", {
  expect_equal(length(distance_statistic(D1,D2,Dpred1,Dpred2)), 1)
})

epsilon = .4
E <- generate_abc_sample (observed_data,
                                    priors,
                                    data_generating_model,
                                    distance_statistic,
                                    epsilon)
test_that("generate_abc_sample output has correct length", {
  expect_equal(length(E) , 4)
})

#We just check one simulation, although it is very simple, it meets
#all the requirements for the testing.


