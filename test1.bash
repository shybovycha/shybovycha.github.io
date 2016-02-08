$ git clone .../project_name.git
$ cd project_name
$ [sudo] bundle install
$ cat config/database.yml
$ # create database and/or change config/database.yml settings
$ rake db:migrate RAILS_ENV=production
$ rake db:seed RAILS_ENV=production # don't worry if one fails
$ # start the server of your choice
