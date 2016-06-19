#initHelperFunctions.r
#helper that initializes random discrete distributions and random
#row-stochastic matrices
generateRandomTransitionMat <- function(numStates){
    #helper that generates  a random row-stochastic transition matrix for us for
    #model initialization (prior to training)
    randomTransitionMat = matrix(0,nrow = numStates,ncol = numStates)
    #generate each matrix component
    for (i in 1:numStates){
        #this gives us our row
        for (j in 1:numStates){
            #column
            if (j == 1){
                #just set our section to a realization of unif(0,1)
                randomTransitionMat[i,j] = runif(1,0,1)
            }
            else if (j > 1 & j < numStates){
                #generate based on previous sums
                sumPrev = sum(randomTransitionMat[i,1:(j-1)])
                randomTransitionMat[i,j] = runif(1,0,1-sumPrev)
            }
            else {
                #calculate based on previous values
                sumPrev = sum(randomTransitionMat[i,1:(j-1)])
                randomTransitionMat[i,j] = 1 - sumPrev
            }
        }
    }
    return(randomTransitionMat)
}
generateRandomInitStateVec <- function(numStates){
    #generate initial state vector randomly
    initStateVec = rep(0,numStates)
    for (i in 1:numStates){
        if (i == 1){
            #generate first one arbitrarily
            initStateVec[i] = runif(1,0,1)
        }
        else if (i > 1 & i < numStates){
            #generate based on limitations set my previous section
            sumPrev = sum(initStateVec[1:(i-1)])
            initStateVec[i] = runif(1,0,1-sumPrev)
        }
        else {
            sumPrev = sum(initStateVec[1:(i-1)])
            initStateVec[i] = 1 - sumPrev
        }
    }
    return(initStateVec)
}