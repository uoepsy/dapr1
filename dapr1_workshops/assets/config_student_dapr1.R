
# get current week:
project_config <- yaml::read_yaml("_quarto.yml")
current_block <- project_config$current_block

# get files 
week_f <- list.files(pattern = "^[0-9]+ex\\.qmd$")

blocks <- list(
  paste0(sprintf("%02d",1:5),"ex.qmd"),
  paste0(sprintf("%02d",6:10),"ex.qmd"),
  paste0(sprintf("%02d",11:15),"ex.qmd"),
  paste0(sprintf("%02d",16:20),"ex.qmd")
)


# sol status for each week
sol_state <- list()
for (wf in week_f) {
  sol_state[[tools::file_path_sans_ext(wf)]] <- 
    which(sapply(blocks, \(x) any(grepl(wf, x)))) < current_block
}


visible <- paste0(names(unlist(sol_state))[which(unlist(sol_state))], collapse = ",")
visible <- ifelse(visible=="", "NONE", visible)

message("Current Block Set To ", current_block)
message("Solutions visible for ", visible)


# save out (gets read by .qmdss when rendreing)
saveRDS(list(current_block = current_block, sols = sol_state), "block_sols.rds")
