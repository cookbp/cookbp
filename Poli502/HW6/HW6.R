# Set a seed for reproducibility
set.seed(1)

# Define the number of people
n_people <- 100

# Number of simulations (the larger the number, the more accurate the estimate)
n_simulations <- 10000

# Initialize counter for matches (rooms where at least 3 people share the same birthday)
n_matches <- 0

# Simulation loop
for(sim in 1:n_simulations) {
  
  # Generate random birthdays for 100 people (each birthday is a number between 1 and 365)
  birthdays <- sample(365, n_people, replace = TRUE)
  
  
  
  # Count the frequency of each birthday
  birthday_counts <- table(birthdays)
  
  # Check if any birthday appears 3 or more times
  if(any(birthday_counts >= 3)) {
    n_matches <- n_matches + 1  # Increment the match counter if condition is true
  }
}

# Calculate the estimated probability
probability <- n_matches / n_simulations

# Print the result
print(paste("Estimated probability that at least three people share the same birthday:", probability))

# [1] "Estimated probability that at least three people share the same birthday: 0.6495"