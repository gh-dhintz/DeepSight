# Define the params list correctly using list() instead of c()
params <- list(
  list(plotA_x_1 = "hp", plotA_x_2 = "disp"),
  list(plotB_x_1 = "wt"),
  list(bookmark_url = "https://your-url.com")
)

# Loop through each sublist and assign variables in the global environment
for (sublist in params) {
  for (name in names(sublist)) {
    assign(name, sublist[[name]], envir = .GlobalEnv)
  }
}

# Check the assigned variables
print(plotA_x_1)    # "hp"
print(plotA_x_2)    # "disp"
print(plotB_x_1)    # "wt"
print(bookmark_url) # "https://your-url.com"