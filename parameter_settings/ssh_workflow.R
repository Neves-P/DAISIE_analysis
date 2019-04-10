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
    ssh_disconnect(connection); gc()
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
    ssh_disconnect(connection); gc()
  }
  return()
}


#' @title Experiment setups
#' @author Giovanni Laudanno
#' @description Experiment setups
#' @inheritParams default_params_doc
#' @return Experiment setups
experiment_setup <- function(
  time = c(2, 3, 6),
  M = 1000,
  lac = c(7.48223*10^-6, 0.0000224467, 0.0000748223),
  mu = c(1),
  K = c(100 / 13500, 30 / 13500, 10 / 13500),
  gam = c(0.001),
  laa = c(1),
  island_ontogeny = "beta",
  replicates = 1000,
  mu_min = 0.1,
  mu_max = mu_min + mu_min * .1,
  Amax = c(13500),
  Apeak = c(0.1),
  Asharpness = c(1),
  Atotalage = c(9)
) {
  if (island_ontogeny == "beta" || island_ontogeny == 2) {
    setups <- expand.grid(
      time,
      M,
      lac,
      mu,
      K,
      gam,
      laa,
      replicates,
      mu_min,
      mu_max,
      Amax,
      Apeak,
      Asharpness,
      Atotalage
    )
    colnames(setups) <- c(
      "time",
      "M",
      "lac",
      "mu",
      "K",
      "gam",
      "laa",
      "replicates",
      "mu_min",
      "mu_max",
      "Amax",
      "Apeak",
      "Asharpness",
      "Atotalage"
    )
  } else {
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
  }
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
  partition = "gelifes",
  time = c(2, 3, 6),
  M = 1000,
  lac = c(7.48223*10^-6, 0.0000224467, 0.0000748223),
  mu = c(1),
  K = c(100 / 13500, 30 / 13500, 10 / 13500),
  gam = c(0.001),
  laa = c(1),
  island_ontogeny = "beta",
  replicates = 1000,
  mu_min = 0.1,
  mu_max = mu_min + mu_min * .1,
  Amax = c(13500),
  Apeak = c(0.1),
  Asharpness = c(1),
  Atotalage = c(9),
  branch = "@develop"
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

  right_setup <- experiment_setup(
    time = time,
    M = M,
    lac = lac,
    mu = mu,
    K = K,
    gam = gam,
    laa = laa,
    island_ontogeny = island_ontogeny,
    replicates = replicates,
    mu_min = mu_min,
    mu_max = mu_max,
    Amax = Amax,
    Apeak = Apeak,
    Asharpness = Asharpness,
    Atotalage = Atotalage
  )

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
    "./", file.path(
      project_name,
      "install_packages.bash"),
    " 'rsetienne/",
    project_name,
    branch,
    "'"
  ))

  if (island_ontogeny == "beta" || island_ontogeny == 2) {
    ssh_exec_wait(session = connection, command = "sleep 5")
    bash_file <- file.path(
      project_name,
      paste0(project_name, "_single_seed.bash")
    )
  } else if (island_ontogeny == "const" || island_ontogeny == 0) {
    ssh_exec_wait(session = connection, command = "sleep 5")
    bash_file <- file.path(
      project_name,
      paste0(project_name, "_0_single_seed.bash")
    )
  }
  ssh_exec_wait(session = connection, command = paste0("cat ", bash_file))

  if (island_ontogeny == "beta") {
    for (setup_number in seq_along(right_setup[,1])) {
      seed <- 1
      ssh_exec_wait(session = connection, command = "sleep 5")
      ssh_exec_wait(session = connection, command = paste(
        "sbatch ",
        bash_file,
        seed,
        right_setup[setup_number,1],
        right_setup[setup_number,2],
        right_setup[setup_number,3],
        right_setup[setup_number,4],
        right_setup[setup_number,5],
        right_setup[setup_number,6],
        right_setup[setup_number,7],
        right_setup[setup_number,8],
        right_setup[setup_number,9],
        right_setup[setup_number,10],
        right_setup[setup_number,11],
        right_setup[setup_number,12],
        right_setup[setup_number,13],
        right_setup[setup_number,14],
        partition,
        sep = " "
      ))
    }
  } else {
    for (setup_number in seq_along(right_setup[,1])) {
      seed <- 1
      ssh_exec_wait(session = connection, command = "sleep 5")
      ssh_exec_wait(session = connection, command = paste(
        "sbatch ",
        bash_file,
        seed,
        right_setup[setup_number,1],
        right_setup[setup_number,2],
        right_setup[setup_number,3],
        right_setup[setup_number,4],
        right_setup[setup_number,5],
        right_setup[setup_number,6],
        right_setup[setup_number,7],
        right_setup[setup_number,8],
        partition,
        sep = " "
      ))
    }
  }
  ssh_disconnect(connection); gc()
}

#' @title Check jobs on cluster
#' @author Giovanni Laudanno
#' @description Check jobs on cluster
#' @inheritParams default_params_doc
#' @return nothing
check_jobs <- function(account = get_p_number()) {

  # connection

  cluster_address <- paste0(account, "@peregrine.hpc.rug.nl")
  if (!require(ssh)) {install.packages("ssh")}
  connection <- ssh_connect(cluster_address)

  jobs <- capture.output(ssh_exec_wait(session = connection, command = "squeue -u $USER --long"))
  ssh_exec_wait(session = connection, command = "sshare -u $USER")
  invisible(ssh_disconnect(connection)); gc()
  return()
}

#' @title Download the results to the results folder of the project
#' @author Giovanni Laudanno
#' @description Download the results to the results folder of the project
#' @inheritParams default_params_doc
#' @return nothing
download_results <- function(
  project_name = get_project_name()
) {

  project_folder <- get_project_folder(project_name)
  remote_results_folder <- file.path(get_project_name(), "results")
  local_results_folder <- file.path(project_folder, "results")
  testit::assert(dir.exists(local_results_folder))

  # download files
  if (!require(ssh)) {install.packages("ssh")}
  accounts <- get_p_number()
  for (account in accounts) {
    cluster_address <- paste0(account, "@peregrine.hpc.rug.nl")
    connection <- ssh_connect(cluster_address)

    system.time(
      scp_download(
        session = connection,
        files = paste0(remote_results_folder, "/*"),
        to = local_results_folder
      )
    )
    ssh_disconnect(connection); gc()
  }
  return()
}

#' @title Download the data to the data folder of the project
#' @author Giovanni Laudanno
#' @description Download the data to the results folder of the project
#' @inheritParams default_params_doc
#' @return nothing
download_data <- function(
  project_name = get_project_name()
) {

  project_folder <- get_project_folder(project_name)
  remote_data_folder <- file.path(get_project_name(), "data")
  local_data_folder <- file.path(project_folder, "data")
  testit::assert(dir.exists(local_data_folder))

  # download files
  if (!require(ssh)) {install.packages("ssh")}
  accounts <- get_p_number()
  for (account in accounts) {
    cluster_address <- paste0(account, "@peregrine.hpc.rug.nl")
    connection <- ssh_connect(cluster_address)

    system.time(
      scp_download(
        session = connection,
        files = paste0(remote_data_folder, "/*"),
        to = local_data_folder
      )
    )
    ssh_disconnect(connection); gc()
  }
  return()
}

load_DAISIE_data <- function() {
  project_name <- get_project_name()
  project_folder <- get_project_folder(project_name)
  platform <- .Platform$OS.type

  if (platform == "windows") {
    data_folder <- local_data_folder <- file.path(project_folder, "data")
    testit::assert(dir.exists(data_folder))
  } else {
    data_folder <- file.path(get_project_name(), "data")
  }

  files <- list.files(data_folder)
  for (file in seq_along(files)) {
    load(file = file.path(local_data_folder, files[file]))
    assign(paste0("sim_", file), out) #nolint
    assign(paste0("args_", file), args) #nolint
  }
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
get_summary_stats <- function() {
  project_name <- get_project_name()
  project_folder <- get_project_folder(project_name)
  platform <- .Platform$OS.type

  if (platform == "windows") {
    data_folder <- local_data_folder <- file.path(project_folder, "data")
    testit::assert(dir.exists(data_folder))
  } else {
    data_folder <- file.path(get_project_name(), "data")
  }

  files <- list.files(data_folder)
  for (file in seq_along(files)) {
    load(
      file = file.path(local_data_folder, files[file])
    )
    assign(paste0("sim_", file), out) #nolint
    assign(paste0("args_", file), args) #nolint
  }

  # Calc median of indep clades
  datasets <- ls(pattern = "sim")
  n_indep_clade <- c()
  medians_indep_clades <- c()
  medians_n_spec_island <- c()
  n_spec_island <- c()
  for (dataset_id in seq_along(datasets)) {
    # Calc median of indep clades
    for (repl in seq_along(get(datasets[dataset_id]))) {
      n_indep_clade[repl] <- # Count number of indep clades using last line
        get(datasets[dataset_id])[[repl]][[1]]$stt_all[
          nrow(get(datasets[dataset_id])[[repl]][[1]]$stt_all), 5]
    }

    # Calc median of number of species
    for (repl in seq_along(get(datasets[dataset_id]))) {
      n_spec_island[repl] <- sum(get(datasets[dataset_id])[[repl]][[1]]$stt_all[
        nrow(get(datasets[dataset_id])[[repl]][[1]]$stt_all),2:4])
    }
    medians_indep_clades[dataset_id] <- median(n_indep_clade)
    medians_n_spec_island[dataset_id] <- median(n_spec_island)
  }
  return(list(medians_indep_clades, medians_n_spec_island))
}

