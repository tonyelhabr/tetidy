
# Source: personal `{sportspredict}` project.

get_db_conn <-
  function(path) {
    # stopifnot(!file.exists(path))
    DBI::dbConnect(RSQLite::SQLite(), dbname = path)
  }

create_table <-
  function(data,
           conn,
           name = deparse(substitute(data)),
           ...,
           overwrite = TRUE,
           append = !overwrite) {
    insert_into_db(
      data = data,
      conn = conn,
      name = name,
      overwrite = overwrite,
      append = append,
      ...
    )
  }

.var_not_sqlite_compatible <-
  function(x) {
    !is.character(x) & !is.numeric(x)
  }

.create_backup_sqlite <-
  function(path, suffix_backup = format(Sys.time(), "%Y-%m-%d_%H-%M-%S")) {
    stopifnot(file.exists(path))
    file <- tools::file_path_sans_ext(path)
    ext <- tools::file_ext(path)
    path_backup <-
      sprintf("%s-%s.%s", file, suffix_backup, ext)
    if(file.exists(path_backup)) {
      msg <- sprintf("Backup file %s already exists! Are you sure you want to overwrite it?", path_backup)
      stop(msg)
    }
    file.copy(from = path, to = path_backup)
    path_backup
  }

insert_into_db <-
  function(data,
           conn,
           name = deparse(substitute(data)),
           overwrite = FALSE,
           append = !overwrite,
           backup = overwrite,
           ...,
           verbose = TRUE,
           add_record_cols = TRUE,
           col_timestamp = "timestamp_record",
           col_id = "id_record",
           timestamp = format(Sys.time(), "%F %X")) {
    # if(missing(get_db_conn)) {
    #   conn <- get_db_conn()
    # }
    # browser()
    e <- DBI::dbExistsTable(con = conn, name = name)
    if(!e) {
      if(overwrite) {
        if(verbose) {
          msg <- sprintf("No table %s exists. Creating it.", name)
          message(msg)
        }
      } else {
        if(verbose) {
        msg <- sprintf("No table %s exists. Do you mean to create it? (Set `overwrite = TRUE` to create it.)", name)
        warning(msg)
        }
        return(invisible(NULL))
      }
    }

    if(backup) {
      # NOTE: Moved this from outside the `overwrite` clause so it is done no matter what.
      path_db <- conn@dbname
      path_db_backup <- .create_backup_sqlite(path = path_db)
      if(verbose) {
      msg <- sprintf("Backing up database as %s as a precaution.", path_db_backup)
      message(msg)
      }
    }

    if(!overwrite) {
      if(add_record_cols) {

        # NOTE: Make sure that these columns are NOT already in the data to be inserted.
        stopifnot(!(all(c(col_timestamp, col_id) %in% names(data))))
        col_timestamp <- sym(col_timestamp)
        col_id <- sym(col_id)

        data <- mutate(data, !!col_timestamp := timestamp)

        if(e) {
          data_e <-
            DBI::dbReadTable(
              conn = conn,
              name = name
            )

          summ_e <-
            dplyr::summarise(data_e, max = max(!!col_id, na.rm = TRUE))
          val_id_max <- summ_e$max

        } else {
          val_id_max <- 0

        }
        data <- dplyr::mutate(data, !!col_id := val_id_max + dplyr::row_number())
      }
    } else {

      if(add_record_cols) {
        if(e) {
          data_e <-
            DBI::dbReadTable(
              conn = conn,
              name = name
            )

          # NOTE: Make sure that these columns are in the database table.
          stopifnot(all(c(col_timestamp, col_id) %in% names(data_e)))
        }
      }
    }


    data <- dplyr::mutate_if(data, .var_not_sqlite_compatible, as.character)
    data <- as.data.frame(data)

    if(verbose) {
      msg <- sprintf("Writing to table %s at %s.", name, Sys.time())
      message(msg)
    }
    DBI::dbWriteTable(
      conn = conn,
      name = name,
      value = data,
      overwrite = overwrite,
      append = append,
      ...
    )
  }

drop_db_conn <-
  function(conn) {
    DBI::dbDisconnect(conn)
  }

read_from_db <-
  function(conn,
           name,
           ...) {
    e <- DBI::dbExistsTable(con = conn, name = name)
    if(!e) {
      msg <- sprintf("No table \"%s\" exists.", name)
      warning(msg, call. = FALSE)
      return(NULL)
    }
    data <-
      DBI::dbReadTable(
        conn = conn,
        name = name,
        ...
      )
    tibble::as_tibble(data)
  }
