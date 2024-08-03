*self_spec:
+ %{!nostdlib:-nodefaultlibs -nostartfiles}

*link:
+ %{!nostdlib:%{mrtp:%{!shared: \
     -l:certRtp.o \
     -L%:getenv(WIND_BASE /target/usr/lib_cert_rtp/pentium/PENTIUM4) \
   }}}
