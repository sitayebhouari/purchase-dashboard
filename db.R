library(DBI)
library(RPostgres)

get_db_connection <- function() {

  con <- dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv('DB_HOST', 'aws-1-eu-west-1.pooler.supabase.com'),
    port = as.integer(Sys.getenv('DB_PORT', '5432')),
    dbname = Sys.getenv('DB_NAME', 'postgres'),
    user = Sys.getenv('DB_USER', 'postgres.vakqfnisbvummieggpcx'),
    password = Sys.getenv('DB_PASSWORD'),
    sslmode = 'require'
  )

  return(con)
}

test_db_connection <- function() {

  con <- NULL

  tryCatch({

    con <- get_db_connection()

    result <- dbGetQuery(
      con,
      'SELECT current_database() AS database, current_user AS username;'
    )

    print(result)
    return(TRUE)

  }, error = function(e) {

    message('Database connection error:')
    message(conditionMessage(e))
    return(FALSE)

  }, finally = {

    if (!is.null(con) && dbIsValid(con)) {
      dbDisconnect(con)
    }

  })
}
