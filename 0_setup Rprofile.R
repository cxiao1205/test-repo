site_path = R.home(component = "home")
fname = file.path(site_path, "etc", "Rprofile.site")
file.exists(fname)
file.exists("~/.Rprofile")
if(!file.exists("~/.Rprofile")) # only create if not already there
  file.create("~/.Rprofile")    # (don't overwrite it)
file.edit("~/.Rprofile")
