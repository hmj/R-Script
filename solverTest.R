#'
#' con_member.csv
#' col = c("DiscreteTime","Member","SumTime", "Time", "AssignedTime")
#'
#' con_project.csv
#' col=c("Project","Class","Difficulty","Priority","Time","Start","End")
#'
#' assigned.csv
#' col = c("DiscreteTime","Member","SumTime", "Time", "AssignedTime")
#'
#'
#' Incompatible
#'  Difficulty
#'  skills Member, Project
#'  Member count
#' 


# library
library(plyr)
library(lpSolve)

# Load DataFiles
setwd("R/")
con.project <- read.csv("con_project.csv", header=TRUE)
con.member <- read.csv("con_member.csv", header=TRUE)
inputSkills <- read.csv("skills.csv", header=TRUE)
assigned <- read.csv("assigned.csv", header=TRUE)

# Count
TimeLines <- sort(as.Date(levels(con.member$DiscreteTime))) ; N.TimeLines <- length(TimeLines)

Members <- sort(levels(con.member$Member)) ; N.Members <- length(Members)

c1 <- levels(con.project$Project)
c2 <- levels(con.project$Class)
N <- length(c1) * length(c2)
c <- c(1:N)
k <- 1
for (i in 1:length(c1)){
  for (j in 1:length(c2)){
    c[k] <- paste(c1[i], c2[j], sep="-")
    k <- k+1
  }
}
Projects <- sort(c) ; N.Projects <- length(Projects)

# Create on Constraints : Delivery matrix -> penalty
penalty <- matrix(-1000, nrow = N.TimeLines, ncol = N.Projects)
colnames(penalty) <- Projects
rownames(penalty) <- 1:N.TimeLines

# Project-const preSolve
for (i in 1:nrow(assigned)){
  for (j in 1:nrow(con.project)){
    if (as.character(assigned[i, "Project"]) == as.character(con.project[j, "Project"])){
      con.project[j, "Time"] <- con.project[j, "Time"] - assigned[i, "AssignedTime"]
    }
  }
}

# Member-const preSolve
for (i in 1:nrow(assigned)){
  for (j in 1:nrow(con.member)){
    if ((as.Date(assigned[i, "DiscreteTime"]) == as.Date(con.member[j, "DiscreteTime"])) &&
        (as.character(assigned[i, "Member"]) == as.character(con.member[j, "Member"]))){
      con.member[j, "Time"] <- con.member[j, "Time"] - assigned[i, "AssignedTime"]
    }
  }
}

# Create on "penalty matrix"
for (i in 1:N.Projects){
  projectCurrent <- Projects[i]
  projectCurrent.name <- strsplit(projectCurrent, "-")[[1]][1]
  projectCurrent.class <- strsplit(projectCurrent, "-")[[1]][2]
  start <- t(con.project[con.project$Project == projectCurrent.name, 6:7])["Start", 1]
  end <- t(con.project[con.project$Project == projectCurrent.name, 6:7])["End", 1]
  term <- which(TimeLines == start):which(TimeLines == end)
  penalty[term, projectCurrent] <- (length(term):1) * con.project[con.project$Project == projectCurrent.name & con.project$Class == projectCurrent.class, "Priority"] # Set Delivery
}

row.signs <- rep("<=", N.TimeLines)
row.rhs <- ddply(con.member, .(as.Date(DiscreteTime)), summarise, sum=sum(Time))[, "sum"]
col.signs <- rep("==", N.Projects)
col.rhs <- con.project[Projects == Projects, "Time"]

# Run
solve.atlas <- lp.transport (penalty, "max", row.signs, row.rhs, col.signs, col.rhs)
solve.atlas$solution

# To continue processing on success
if (solve.atlas$status == 0){
  output <- data.frame()
  
  m <- solve.atlas$solution
  skills <- data.matrix(inputSkills[, -1])
  
  #result : outputdata
  result <- array(0, dim = c(N.Members, N.Projects, N.TimeLines))
  
  for (i in 1:N.TimeLines){
    timeCurrent <- TimeLines[i]
    
    # Set up constraint signs and right-hand sides.
    row.signs <- rep("<=", N.Members)
    row.rhs <- arrange(subset(con.member, as.Date(DiscreteTime) == as.Date(timeCurrent)), order(Member))[, "Time"]
    col.signs <- rep("==", N.Projects)
    col.rhs <- m[i, ]
    
    lp.temp <- lp.transport (skills, "max", row.signs, row.rhs, col.signs, col.rhs)
    result[, , i] <- lp.temp$solution
  }
  
  # Create on output
  col.res <- cbind(result)
  col.member <- cbind(rep(Members, N.TimeLines * N.Projects))
  col.project <- cbind(rep(sort(rep(Projects, N.Members)), N.TimeLines))
  col.time <- sort(rep(as.Date(TimeLines), N.Members * N.Projects))
  output <- cbind(as.character(col.time), col.member, col.project, col.res)
  output <- data.frame(DiscreteTime=as.Date(col.time), Member=col.member, Project=col.project, ReqTime=col.res)
  colnames(output) <- c("DiscreteTime", "Member", "Project", "Time")

  # Merge output to assigned
  output <- merge(output, assigned, all=T)
  output[is.na(output)] <- 0
  output$SumTime <- output$Time + output$AssignedTime

  # Data shaping`
  output <- output[, c("DiscreteTime", "Member", "Project", "SumTime", "Time", "AssignedTime")]
  
  # output csv
  write.csv(output, file="output2.csv", row.names = TRUE)
}
