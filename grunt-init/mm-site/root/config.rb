
###
## Sprockets bower config
####
ready do
  @bower_config = JSON.parse(IO.read("#{root}/.bowerrc"))
  sprockets.append_path File.join(root, @bower_config["directory"])
end

# no html extension
activate :directory_indexes

set :css_dir, 'stylesheets'

set :js_dir, 'javascripts'

set :images_dir, 'images'

# use debugging javascripts
set :debug_assets, true

# Build-specific configuration
configure :build do
  # For example, change the Compass output style for deployment
  activate :minify_css

  # Minify Javascript on build
  activate :minify_javascript

  # Enable cache buster
  # activate :asset_hash

  # Use relative URLs
  # activate :relative_assets

  # Or use a different image path
  # set :http_prefix, "/Content/images/"
end

# Deployment
#activate :deploy do |deploy|
  #deploy.build_before = true

  #deploy.method = "rsync"
  #deploy.host = ""
  #deploy.path = ""
  #deploy.user = ""
  #deploy.clean = true
#end

