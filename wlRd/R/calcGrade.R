# compute BIBI grade from BIBI score
calcGrade <- function(score = Overall.Score){

    ifelse(score < 20, 'very poor', ifelse(score < 40, 'poor', ifelse(
      score < 60, 'fair', ifelse(score < 80, 'good', 'excellent'))))
  }
