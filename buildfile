# Generated by Buildr 1.3.3, change to your liking
# Version number for this release
VERSION_NUMBER = "1.0.0"
# Group identifier for your projects
NEXT_VERSION = "1.0.1"

GROUP = "SQuery"
COPYRIGHT = "GPL"

require 'buildr/scala' if Buildr::VERSION =~ /1.3.3/i

# Specify Maven 2.0 remote repositories here, like this:
repositories.remote << "http://www.ibiblio.org/maven2/"

my_layout = Layout.new
my_layout[:source, :main, :scala] = 'src'

desc "The Squery project"
define "SQuery",:layout => my_layout do
  project.version = VERSION_NUMBER
  project.group = GROUP
  manifest["Implementation-Vendor"] = COPYRIGHT
  package :jar
end