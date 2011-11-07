# ------------------------------------------------------------------------------
# SERVER
# ------------------------------------------------------------------------------

cqi_server <- function(userflags, port=4877, host="localhost") {

  user = "cqi_server";
  passwd = paste("pass", round(runif(1) * 100), sep="");

  cqpserver = "cqpserver";
  flags = "-1 -L -q ";      # single-client server, localhost only (for security reasons)
  flags = paste(flags, userflags);         # append optional command-line flags

  # generate temporary user list file for CQPserver
  passfile = "/tmp/CQIServer";
  cat(paste("user ", user, " \"", passwd, "\";\n", sep=""), file=passfile);;
  system(command=paste("chmod 0600", passfile));

  ## TODO
  #  # scan for free port (using rand() so two servers invoked at the same time won't collide)
  #  my $port = 10000 + int rand(2000);
  #  my %in_use = 
  #    map {$_ => 1}
  #      map {(/\*\.([0-9]+)/) ? $1 : 0}
  #        `netstat -a | grep LISTEN`;
  #  while ($port < 60000 and $in_use{$port}) {
  #    $port += rand(20);          # jump randomly to avoid collisions
  #  }
  #  croak "Can't find free port for CQPserver. Abort."
  #    unless $port < 60000;

  # now start CQPserver on this port
  #cmd <- paste("cqpserver", flags, "-P", port, "-I", passfile, ">/dev/null 2>&1");
  cmd <- paste("cqpserver", flags, "-P", port, "-I", passfile);
  print(cmd);
  ok <- system(command=cmd, wait=FALSE, intern=FALSE);
  if (ok != 0) {
    stop("CQPserver failed to launch");
  }

  # delete user list file
  #system(paste("rm", passfile));

  # return connection information suitable for cqi_connect()
  r <- c(user=user, passwd=passwd, host=host, port=port);
  return(r);
}

