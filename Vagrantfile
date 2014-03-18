# Vagrantfile API/syntax version. Don't touch unless you know what you're doing!
VAGRANTFILE_API_VERSION = "2"

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|

    # Every Vagrant virtual environment requires a box to build off of.
    config.vm.box = "precise64"

    # The url from where the 'config.vm.box' box will be fetched if it
    # doesn't already exist on the user's system.
    config.vm.box_url = "http://files.vagrantup.com/precise64.box"

    # Compare the box against this checksum to confirm
    # that no corruption occured during the download process.
    # checksum = "9a8bdea70e1d35c1d7733f587c34af07491872f2832f0bc5f875b536520ec17e"
    # config.vm.box_download_checksum = checksum
    # config.vm.box_download_checksum_type = "sha256"

    # How provisioning should be done.
    config.vm.provision :shell, :path => "./scripts/bootstrap.sh"
    config.vm.provision :shell, :path => "./scripts/install-gnat.sh"
    config.vm.provision :shell, :path => "./scripts/install-valgrind.sh"

    # Open up the VM to the local network.
    # Disabled this by default as it will require the user to manually
    # answer a question after download and installation of the base
    # box but before configuration.
    # config.vm.network "public_network" # Bridged network.

    # Map the hosts folder with the Vagrant file to /home/vagrant/host on the
    # Virtual Machine. Disable default mapping.
    config.vm.synced_folder ".", "/vagrant", :disabled => true
    config.vm.synced_folder "./", "/home/vagrant/host"

    # Using VirtualBox:
    config.vm.provider :virtualbox do |vb|
       # Don't boot with headless mode
       # vb.gui = true
       # vb.customize ["modifyvm", :id, "--accelerate3d", "off"]

       ### CPU and memory settings
       # Recommended settings
       vb.customize ["modifyvm", :id, "--memory", "4096"]
       vb.customize ["modifyvm", :id, "--cpus", "4"]

       # Low-end settings
       # vb.customize ["modifyvm", :id, "--memory", "512"]
       # vb.customize ["modifyvm", :id, "--cpus", "1"]
     end
end
