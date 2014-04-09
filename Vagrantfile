# -*- mode: ruby -*-
# vi: set ft=ruby :

# Vagrantfile API/syntax version. Don't touch unless you know what you're doing!
VAGRANTFILE_API_VERSION = "2"

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|
  config.vm.box = "ubuntu13.04-server-amd64"
  config.vm.box_url = "http://mkulkin.srt.mirantis.net:8081/ubuntu13.04-server-amd64.box"

  config.vm.network "forwarded_port", guest: 3000, host: 3000
  config.vm.network "private_network", ip: "10.0.1.10"
  config.vm.synced_folder ".", "/vagrant", nfs: true

  config.vm.provider :virtualbox do |vb|
    vb.customize ["modifyvm", :id, "--cpus", "2"]
    vb.customize ["modifyvm", :id, "--memory", "1024"]
  end
  
  config.vm.provision :chef_solo do |chef|
    chef.cookbooks_path = ["vagrant/cookbooks"]

    chef.add_recipe "zendesk"
  end
end
