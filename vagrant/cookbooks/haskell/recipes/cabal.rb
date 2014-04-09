cabal_version = '1.18.0.2'

remote_file "#{Dir.tmpdir}/cabal-install-#{cabal_version}.tar.gz" do
  source "http://hackage.haskell.org/package/cabal-install-1.18.0.2/cabal-install-#{cabal_version}.tar.gz"
end

bash 'Install cabal' do
  code <<-EOS
tar -xf #{Dir.tmpdir}/cabal-install-#{cabal_version}.tar.gz
cd cabal-install-#{cabal_version}
sudo sh bootstrap.sh --global
cd ..
rm -rf cabal-install-#{cabal_version}
  EOS
  cwd Dir.tmpdir

  creates '/usr/local/bin/cabal'
end

template '/etc/profile.d/cabal.sh' do
  source 'cabal.sh.erb'
  mode '644'
end
