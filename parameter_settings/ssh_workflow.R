#' Get your p number
#'
#' @return a string with p number
#' @export
get_p_number <- function() {
  p_number <- "p282067"
  p_number
}

#' Get name of project you're working on
#'
#' @return String with project name
#' @export
get_project_name <- function(project = "DAISIE") {
  assertthat::is.string(project)
  project_name <- project
  project_name
}


#' Get path of projects superfolder
#'
#' @return String with path of superfolder.
#' @export
#'
#' @examples
get_super_folder <- function() {
  super_folder <- file.path("D:", "Projects")
  super_folder
}

#' @title Get project folder address
#' @author Giovanni Laudanno
#' @description Get project folder address
#' @inheritParams default_params_doc
#' @return project folder address
get_project_folder <- function(project_name = get_project_name()) {
  db_dir <- get_super_folder()
  home_dir <- db_dir
  # home_dir <- file.path(db_dir, get_project_name())
  home_files <- list.files(paste0(home_dir))
  project_folder <- home_files[which(grepl(
    pattern = project_name,
    x = home_files
  ))]
  project_folder <- file.path(home_dir, project_folder)
  project_folder
}

#' @title Download the results to the results folder of the project
#' @author Giovanni Laudanno
#' @description Download the results to the results folder of the project
#' @inheritParams default_params_doc
#' @return nothing
download_results <- function(
  project_name = get_project_name(), p_numbers = get_p_number()
) {

  project_folder <- get_project_folder(project_name)
  remote_results_folder <- file.path(project_name, "results")
  local_results_folder <- file.path(project_folder, "results")
  testit::assert(dir.exists(local_results_folder))

  # download files
  while (!require(ssh)) {install.packages("ssh")}
  accounts <- p_numbers
  for (account in accounts) {
    cluster_address <- paste0(account, "@peregrine.hpc.rug.nl")
    connection <- ssh_connect(cluster_address)

    ssh::ssh_exec_wait(
      connection,
      command = paste0("mkdir -p ", project_name)
    )
    ssh::ssh_exec_wait(
      connection,
      command = paste0("mkdir -p ", project_name, "results")
    )

    system.time(
      ssh::scp_download(
        session = connection,
        files = paste0(remote_results_folder, "/*"),
        to = local_results_folder
      )
    )
    rm(connection); gc()
  }
  return()
}

#' @title Read saved results
#' @author Giovanni Laudanno
#' @description Read saved results
#' @inheritParams default_params_doc
#' @return results
read_results <- function(project_name = get_project_name()) {
  project_folder <- get_project_folder(project_name)
  if (is.null(project_folder)) {
    if (.Platform$OS.type == "windows") {
      project_folder <- system.file("extdata", package = get_project_name())
    }
  }
  if (!dir.exists(project_folder)) {
    stop("This directory does not exist")
  }
  if (length(list.files(project_folder)) == 0) {
    stop(paste0(project_folder, " is empty."))
  }
  dir_results <- file.path(project_folder, "results")
  files_results <- c(
    list.files(dir_results, pattern = ".txt"),
    list.files(dir_results, pattern = ".csv")
  )
  if (length(files_results) > 0) {
    if (length(files_results) == 0) {
      stop(paste0(dir_results, " is empty."))
    }
    all_results <- data.frame()
    for (file_results in files_results) {
      x <- utils::read.csv(
        file.path(
          dir_results,
          file_results
        )
      )
      if (tools::file_ext(file_results) == "txt") {
        x <- x[, -1]
      }
      all_results <- rbind(all_results, x)
    }
    return(all_results)
  } else {
    return(NULL)
  }
}

#' @title Export cluster scripts
#' @author Giovanni Laudanno
#' @description Export cluster scripts
#' @inheritParams default_params_doc
#' @return nothing
upload_cluster_scripts <- function(
  project_name = get_project_name(),
  p_numbers = get_p_number()
) {

  accounts <- p_numbers

  for (account in accounts) {

    cluster_address <- paste0(account, "@peregrine.hpc.rug.nl")
    while (!require(ssh)) {install.packages("ssh")}
    connection <- ssh_connect(cluster_address)

    # folder structure
    project_folder <- get_project_folder(project_name)
    remote_project_folder <- file.path(project_name)
    local_cluster_folder <- file.path(project_folder, "cluster_scripts")
    testit::assert(dir.exists(local_cluster_folder))

    ssh_exec_wait(connection, command = paste0("mkdir -p ", project_name))
    # ssh_exec_wait(connection, command = paste0(
    #   "mkdir -p ",
    #   file.path(project_name, "results", fsep = "/")
    # ))
    system.time(
      scp_upload(
        session = connection,
        files = paste0(
          local_cluster_folder,
          "/",
          list.files(local_cluster_folder, pattern = ".bash")
        ),
        to = remote_project_folder
      )
    )
    rm(connection); gc()
  }
  return()
}


#' @title Experiment setups
#' @author Giovanni Laudanno
#' @description Experiment setups
#' @inheritParams default_params_doc
#' @return Experiment setups
experiment_setup <- function() {
  time <- c(2, 3, 5)
  M <- 1000
  lac <- c(1)
  mu <- c(0.5)
  K <- c(20)
  gam <- c(0.009)
  laa <- c(1)
  replicates <- 20
  setups <- expand.grid(
    time,
    M,
    lac,
    mu,
    K,
    gam,
    laa,
    replicates
  )
  colnames(setups) <- c(
    "time",
    "M",
    "lac",
    "mu",
    "K",
    "gam",
    "laa",
    "replicates"
  )
  setups
}


#' @title Execute next setup
#' @author Giovanni Laudanno
#' @description Execute next setup
#' @inheritParams default_params_doc
#' @return nothing
execute_next_setup <- function(
  project_name = get_project_name(),
  max_sims = 1000,
  account = get_p_number(),
  download_files = TRUE,
  partition = "gelifes"
) {

  if (!(partition == "gelifes" || partition == "regular")) {
    stop("This is not a legitimate cluster partition")
  }

  project_folder <- get_project_folder(project_name)

  # download files
  if (download_files == FALSE) {
    download_results(project_name = project_name)
  }

  # upload scripts
  upload_cluster_scripts(project_name = project_name)

 right_setup <- experiment_setup()[1,]


  if (is.null(project_folder)) {
    if (.Platform$OS.type == "windows") {
      project_folder <- system.file("extdata", package = project_name)
    }
  }
  if (!dir.exists(project_folder)) {
    stop("This directory does not exist")
  }
  if (length(list.files(project_folder)) == 0) {
    stop(paste0(project_folder, " is empty."))
  }

  cluster_address <- paste0(account, "@peregrine.hpc.rug.nl")
  if (!require(ssh)) {install.packages("ssh")}
  connection <- ssh_connect(cluster_address)

  ssh_exec_wait(session = connection, command = paste0(
    "chmod +x ", file.path(project_name, "install_packages.bash")
  ))
  ssh_exec_wait(session = connection, command = paste0(
    "./", file.path(project_name, "install_packages.bash")," 'rsetienne/", project_name,"@develop","'"
  ))
  ssh_exec_wait(session = connection, command = "sleep 5")
  bash_file <- file.path(
    project_name,
    paste0(project_name, "_single_seed.bash")
  )
  ssh_exec_wait(session = connection, command = paste0("cat ", bash_file))
  seed <- 1
    ssh_exec_wait(session = connection, command = paste0(
      "sbatch ",
      bash_file,
      " ",
      seed,
      " ",
      right_setup$time,
      " ",
      right_setup$M,
      " ",
      right_setup$lac,
      " ",
      right_setup$mu,
      " ",
      right_setup$K,
      " ",
      right_setup$gam,
      " ",
      right_setup$laa,
      " ",
      seed,
      " ",
      partition
    ))
  rm(connection); gc()
}
