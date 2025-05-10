generate_worker <- function(id, name, gender, salary) {
  level <- assign_level(salary, gender)
  return(list(id = id, name = name, gender = gender, salary = salary, level = level))
}

assign_level <- function(salary, gender) {
  if (!is.numeric(salary) || salary <= 0) {
    return("Invalid Salary")
  }
  if (salary > 10000 && salary < 20000) {
    return("A1")
  } else if (salary > 7500 && salary < 30000 && tolower(gender) == 'female') {
    return("A5-F")
  } else {
    return("N/A")
  }
}

generate_payment_slip <- function(worker) {
  tryCatch({
    paste("Worker ID:", worker$id, "Name:", worker$name, "Gender:", worker$gender,
          "Salary:", worker$salary, "Level:", worker$level)
  }, error = function(e) {
    paste("Error generating payment slip for Worker ID", worker$id, ":", e$message)
  })
}

create_workers <- function(num_workers) {
  num_workers <- min(num_workers, 400)
  workers <- list()
  genders <- c("Male", "Female")
  
  for (i in 1:num_workers) {
    name <- paste("Employee", i, sep = "_")
    gender <- sample(genders, 1)
    salary <- sample(5000:35000, 1)
    workers[[i]] <- generate_worker(i, name, gender, salary)
  }
  
  return(workers)
}

main <- function() {
  repeat {
    user_input <- readline("Enter the number of workers to generate (max 400, default 400): ")
    if (user_input == "" || grepl("^[0-9]+$", user_input)) {
      num_workers <- as.numeric(user_input)
      if (is.na(num_workers)) num_workers <- 400
      break
    } else {
      cat("Invalid input. Please enter a valid number.\n")
    }
  }
  
  workers <- create_workers(num_workers)
  for (worker in workers) {
    cat(generate_payment_slip(worker), "\n")
  }
}

main()
