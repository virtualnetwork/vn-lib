VN-Lib
=======
Library parts of the "Virtual Network" implementation.
[![Build
Status](https://travis-ci.org/virtualnetwork/vn-lib.png?branch=development)](https://travis-ci.org/virtualnetwork/vn-lib)

### Keywords
Space plug-and-play Avionics, Space plug-and-play Architecture, Ada,
Safety-critical systems, Internet of Things, Unmanned Vehicles

### How to compile
For this project Virtualbox and Vagrant is used to make it easier to set up a
common development environment for all developers. Another reason is that
compiling and configuring the toolchain for Ada on both amd64 and for ARM can
take a lot of valuable time from development.

### Get going on Ubuntu 12.04
To install latest Virtualbox and Vagrant on Ubuntu 12.04 do the following:

**Add virtualbox repository and install Virtualbox 4.3 with apt-get**
```
$ sudo sh -c 'echo "deb http://download.virtualbox.org/virtualbox/debian precise contrib" >> /etc/apt/sources.list'
$ wget -q http://download.virtualbox.org/virtualbox/debian/oracle_vbox.asc -O- | sudo apt-key add -
$ sudo apt-get update
$ sudo apt-get install virtualbox-4.3
```

**Download and install Vagrant**
```
http://www.vagrantup.com/downloads.html
$ sudo dpkg -i <downloaded_deb_file>
```

##### Start the VM and build the code within the VM
```
$ git clone https://github.com/virtualnetwork/vn-lib
$ cd vn-lib
$ vagrant up (wait for the VM to install and boot up)
$ vagrant ssh
$ cd host/
$ gnatmake OR make OR ??? (TODO)
```
### Get going on Windows
TODO

### Get going on OS X
TODO

### Git usage
To easier be able to follow code history [git-flow
methodology](http://nvie.com/posts/a-successful-git-branching-model/) will be
used.  Fork this repository on github and work on features in your own feature
branches as well as fixes in your own "fix"-branches. When the feature/fix is
done and the code compiles do a pull request to the development branch in
repository https://github.com/virtualnetwork/vn-lib

### Coding standards
A good start is the "[Ada Quality and Style
Guide](https://en.wikibooks.org/wiki/Ada_Style_Guide)". The more specific rules
will evolve over time within this project.

### Release versioning
This project will release code following the "[Semantic
versioning](http://semver.org/)" guidelines.

### Suggested reading
1. [ISO Technical Report - Guide for the use of the Ada programming language in
   high integrity
    systems](http://standards.iso.org/ittf/PubliclyAvailableStandards/c029575_ISO_IEC_TR_15942_2000%28E%29.zip)
2. [ISO Technical Report - Guide for the use of the Ada Ravenscar Profile in
   high integrity
    systems](http://standards.iso.org/ittf/PubliclyAvailableStandards/c038828_ISO_IEC_TR_24718_2005%28E%29.zip)
3. ["High-Integrity Object-Oriented Programming in Ada" by
   Adacore](http://www.adacore.com/knowledge/technical-papers/high-integrity-oop-in-ada/)
4. ["Ada Quality and Style Guide" from wikibooks by wikibooks contributors](https://en.wikibooks.org/wiki/Ada_Style_Guide)
