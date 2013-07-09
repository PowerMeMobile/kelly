[![Build Status](https://travis-ci.org/PowerMeMobile/kelly.png?branch=master)](https://travis-ci.org/PowerMeMobile/kelly)
=======
# OpenKelly Installation & Launching

The variable **$WORK** refers to a working directory, such as ~/Work.
Every point described below assumes that the current working directory is **$WORK**.

## 1. Erlang installation

### 1.1 Install Erlang dependencies

#### Ubuntu
<pre>
$ sudo apt-get install build-essential libwxgtk2.8-dev libglu-dev xsltproc libssl-dev libncurses5-dev fop unixodbc-dev default-jdk libtk-img-dev
</pre>
#### CentOS
<pre>
$ sudo yum install gcc gcc-c++ wxGTK-devel libxslt openssl-devel ncurses-devel fop unixODBC-devel java-1.6.0-openjdk-devel
</pre>

### 1.2 Build and install Erlang from sources
<pre>
$ wget http://www.erlang.org/download/otp_src_R15B03-1.tar.gz
$ tar xfz otp_src_R15B03-1.tar.gz
$ cd otp_src_R15B03/
$ ./configure
$ make
$ sudo make install
</pre>


## 2. RabbitMQ installation

### 2.1 Install RabbitMQ dependencies

#### Ubuntu
<pre>
$ sudo apt-get install zip nmap xmlto
</pre>

#### CentOS
<pre>
$ sudo yum install zip unzip nmap xmlto
</pre>

### 2.2 Build and install RabbitMQ from sources
<pre>
$ wget http://www.rabbitmq.com/releases/rabbitmq-server/v2.7.1/rabbitmq-server-2.7.1.tar.gz
$ tar xfz rabbitmq-server-2.7.1.tar.gz
$ cd rabbitmq-server-2.7.1/
$ sudo make install TARGET_DIR=/usr/local/lib/rabbitmq-server SBIN_DIR=/usr/local/lib/rabbitmq-server/sbin MAN_DIR=/usr/local/lib/rabbitmq-server/man
</pre>


## 3. MongoDB installation

### 3.1 Download MongoDB binary package
<pre>
$ wget http://fastdl.mongodb.org/linux/mongodb-linux-x86_64-2.2.2.tgz
$ tar -zxvf mongodb-linux-x86_64-2.2.2.tgz
</pre>

### 3.2 Create db directory
<pre>
$ sudo mkdir /data/db
</pre>

### 3.3 Start MongoDB
<pre>
$ ./mongodb-linux-x86_64-2.2.2/bin/mongod --fork --logpath /data/db/mongo.log --dbpath /data/db/
</pre>

### 3.4 Try to connect to MongoDB instance
<pre>
$ ./mongodb-linux-x86_64-2.2.2/bin/mongo
MongoDB shell version: 2.2.2
connecting to: test
>
</pre>

### 3.5 Setup indexes to MongoDB
<pre>
$ mongo $WORK/kelly/rel/files/indexes.js
</pre>

## 4. Kelly & Funnel & Just installation

### 4.1 Install dependencies

#### Ubuntu
<pre>
$ sudo apt-get install git-core uuid-dev libtokyocabinet-dev libwww-perl
</pre>
#### CentOS
<pre>
$ sudo yum install git-core libuuid-devel tokyocabinet-devel perl-libwww-perl
</pre>

### 4.2 Download Funnel

Funnel is currently available as a binary package. The source code will be available soon.

#### Ubuntu
<pre>
$ wget https://dl.dropbox.com/u/68938861/funnel_mini_ubuntu12.10_x86_64.tar.gz
$ tar xfz funnel_mini_ubuntu12.10_x86_64.tar.gz
</pre>
#### CentOS
<pre>
$ wget https://dl.dropbox.com/u/68938861/funnel_mini_centos6_x86-64.tar.gz
$ tar xfz funnel_mini_centos6_x86-64.tar.gz
</pre>

### 4.3 Build Kelly from sources
<pre>
$ git clone git://github.com/PowerMeMobile/kelly.git -b mongodb_storage
$ cd kelly
$ make
</pre>

### 4.4 Build Just from sources
<pre>
$ git clone git://github.com/PowerMeMobile/just_mini_rel.git
$ cd just_mini_rel
$ make
</pre>

## 5. Startup

You might need to open different tabs for each step or use the `screen' command.

### 5.1 RabbitMQ launching
<pre>
$ sudo /usr/local/lib/rabbitmq-server/sbin/rabbitmq-server -detached
</pre>

### 5.2 Funnel launching
<pre>
$ ./funnel_mini/bin/funnel start
</pre>

### 5.3 Configure Kelly node name.
<pre>
$ vim $WORK/kelly/rel/kelly/releases/1/vm.args
Replace '127.0.0.1' in kelly@127.0.0.1 with a valid server's ip address.
</pre>

### 5.4 Kelly launching
<pre>
$ ./kelly/rel/kelly/bin/kelly start
</pre>

### 5.5 Just launching
<pre>
$ ./just_mini_rel/just_mini/bin/just start
</pre>


5.6 Configuration uplink settings
<pre>
$ cd $WORK/kelly/rel/kelly/bin/
</pre>

Configure the **OUTPUT_SMPP_HOST** and **OUTPUT_SMPP_PORT** settings to your SMPP target in `kelly_http_configure' file.
Apply the settings:
<pre>
$ ./kelly_http_configure
</pre>
