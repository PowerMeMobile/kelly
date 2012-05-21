OpenKelly Installation & Launching

NB:
The variable `$WORK' refers to a working directory, such as ~/Work.
Every point described below assumes that the current working directory is `$WORK'.

1. Erlang installation

1.1 Install Erlang dependencies
$ sudo apt-get install build-essential libwxgtk2.8-dev libglu-dev xsltproc libssl-dev libncurses5-dev fop unixodbc-dev default-jdk libtk-img-dev

1.2 Build and install Erlang from sources
$ wget http://www.erlang.org/download/otp_src_R14B04.tar.gz
$ tar xfz otp_src_R14B04.tar.gz
$ cd otp_src_R14B04/
$ ./configure
$ make
$ sudo make install


2. RabbitMQ installation

2.1 Install RabbitMQ dependencies
$ sudo apt-get install zip nmap xmlto

2.2 Build and install RabbitMQ from sources
$ wget http://www.rabbitmq.com/releases/rabbitmq-server/v2.7.1/rabbitmq-server-2.7.1.tar.gz
$ tar xfz rabbitmq-server-2.7.1.tar.gz
$ cd rabbitmq-server-2.7.1/
$ sudo make install TARGET_DIR=/usr/local/lib/rabbitmq-server SBIN_DIR=/usr/local/lib/rabbitmq-server/sbin MAN_DIR=/usr/local/lib/rabbitmq-server/man


3. Kelly & Funnel & Just installation

3.1 Install dependencies
$ sudo apt-get install git-core uuid-dev libtokyocabinet-dev libwww-perl

3.2 Build Funnel from sources
$ git clone git://github.com/PowerMeMobile/funnel_mini_rel.git
$ cd funnel_mini_rel
$ make

3.3 Build Kelly from sources
$ git clone git://github.com/PowerMeMobile/kelly.git
$ cd kelly
$ make

3.4 Build Just from sources
$ git clone git://github.com/PowerMeMobile/just_mini_rel.git
$ cd just_mini_rel
$ make


4. Startup

NB:
You might need to open different tabs for each step or use the `screen' command.

4.1 RabbitMQ launching
$ sudo /usr/local/lib/rabbitmq-server/sbin/rabbitmq-server -detached

4.2 Funnel launching
$ cd $WORK/funnel_mini_rel/
$ make console
Note the address and port from the console output, you will see something like:
[info] server: initializing (addr: 127.0.1.1, port: 2775)

4.3 Kelly launching
$ cd $WORK/kelly/
$ make console

4.4 Just launching
$ cd $WORK/just_mini_rel/
$ make console

4.5 Configuration
$ cd $WORK/kelly/rel/kelly/bin/

Configure the `OUTPUT_SMPP_HOST' and `OUTPUT_SMPP_PORT' settings to your SMPP target (SMPPSim for example) in `kelly_http_configure' file.
Apply the settings:
$ ./kelly_http_configure
After a while you should see in the Funnel console output something like that:
[info] server: granted bind (addr: 127.0.0.1, customer: test-sys-id, user: user, password: password, type: transceiver, rps: 300)

