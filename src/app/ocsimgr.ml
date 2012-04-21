(*
 * Copyright (c) 2012 Sebastien Mondet <seb@mondet.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)
open Core.Std

let mime_types = "
application/atom+xml		atom
application/mathml+xml		mathml
application/msword		doc
application/ogg			ogg
application/pdf			pdf
application/postscript		ai eps ps
application/rdf+xml		rdf
application/smil		smi smil
application/srgs		gram
application/srgs+xml		grxml
application/vnd.mozilla.xul+xml	xul
application/vnd.ms-excel	xls
application/vnd.ms-powerpoint	ppt
application/x-dvi		dvi
application/x-javascript	js
application/x-koan		skp skd skt skm
application/x-latex		latex
application/x-netcdf		nc cdf
application/x-sh		sh
application/x-shar		shar
application/x-shockwave-flash	swf
application/x-tar		tar
application/x-tcl		tcl
application/x-tex		tex
application/x-texinfo		texinfo texi
application/x-troff		t tr roff
application/x-troff-man		man
application/x-troff-me		me
application/x-troff-ms		ms
application/xhtml+xml		xhtml xht
application/xslt+xml		xslt
application/xml			xml xsl
application/xml-dtd		dtd
application/xml-external-parsed-entity
application/zip			zip
audio/midi			mid midi kar
audio/mpeg			mpga mp2 mp3
audio/x-aiff			aif aiff aifc
audio/x-mpegurl			m3u
audio/x-pn-realaudio		ram ra
application/vnd.rn-realmedia	rm
audio/x-wav			wav
image/bmp			bmp
image/cgm			cgm
image/gif			gif
image/ief			ief
image/jpeg			jpeg jpg jpe
image/png			png
image/svg+xml			svg
image/tiff			tiff tif
image/x-cmu-raster		ras
image/x-icon			ico
image/x-portable-anymap		pnm
image/x-portable-bitmap		pbm
image/x-portable-graymap	pgm
image/x-portable-pixmap		ppm
image/x-rgb			rgb
image/x-xbitmap			xbm
image/x-xpixmap			xpm
image/x-xwindowdump		xwd
model/vrml			wrl vrml
text/calendar			ics ifb
text/css			css
text/html			html htm
text/plain			asc txt
text/richtext			rtx
text/rtf			rtf
text/sgml			sgml sgm
text/tab-separated-values	tsv
video/mpeg			mpeg mpg mpe
video/quicktime			qt mov
video/vnd.mpegurl		mxu m4u
video/x-msvideo			avi
"

let config ?(debug=false) ?ssl ?(port=80) ?(authentication=`pam "login") 
    ~runtime_root ?conf_root ?ssl_dir
    ~static_dirs ?log_root kind output_string =
  let out = output_string in
  let line out fmt = ksprintf out (fmt ^^ "\n") in
  let conf_dir = Option.value ~default:runtime_root conf_root in
  let log_dir = Option.value ~default:runtime_root log_root in
  let extensions =
    match kind with
    | `Ocsigen ->
      sprintf "
    <extension findlib-package=\"ocsigenserver.ext.staticmod\"/>
    <extension findlib-package=\"core\"/>
    <extension module=\"_build/src/simple_pam/simple_pam.cma\"/>
    <extension findlib-package=\"ocsigenserver.ext.ocsipersist-sqlite\">
      <database file=\"%s/ocsidb\"/>
    </extension>
    <extension findlib-package=\"eliom.server\"/> "
        runtime_root
    | `Static ->
      sprintf "
    <extension name=\"staticmod\"/>
    <extension name=\"ocsipersist\">
      <database file=\"%s/ocsidb\"/>
    </extension>
    <extension name=\"eliom\"/>" runtime_root
  in
  let website_module =
    match kind with
    | `Ocsigen ->
      sprintf " \
        <eliom module=\"_build/ocsibase/ocsibase.cma\">\n"
    |`Static ->
      sprintf " <eliom name=\"ocsibase\">\n"
  in
  line out "<ocsigen>\n  <server>\n    <port>%d</port>\n" port;
  Option.iter ssl (fun (cert, key) ->
    let dir = Option.value ~default:conf_dir ssl_dir in
    line out "<port protocol=\"HTTPS\">%d</port>\n" (port - 80 + 443);
    line out "<ssl><certificate>%s/%s</certificate>" dir cert;
    line out "     <privatekey>%s/%s</privatekey></ssl>" dir key;
  );
  line out "    <user></user> <group></group>";
  line out "    <logdir>%s</logdir>" log_dir;
  line out "    <commandpipe>%s/ocsigen_command</commandpipe>" runtime_root;

  line out "    <mimefile>%s/mime.types</mimefile>" conf_dir;
  line out " <charset>utf-8</charset> <debugmode/>";
  line out "%s" extensions;
  line out "<host hostfilter=\"*\">";
  List.iter static_dirs (fun static_dir ->
    line out " <static dir=\"%s/\" />" static_dir;
  );
  line out " %s\n" website_module;
  begin match authentication with
  | `none -> ()
  | `pam s -> 
    line out "  <pam-authentication-service>%s</pam-authentication-service>" s
  end;
  if debug then output_string "<debug/>\n";
  line out "</eliom>";
  line out "</host>";
  line out "  </server>\n</ocsigen>";
  ()

let syscmd s =
  match Unix.system s with
  | `Exited 0 -> Ok ()
  | e -> Error (Failure (Unix.Process_status.to_string_hum e))

let syscmdf fmt =
  ksprintf syscmd fmt

let syscmd_exn s = syscmd s |! Result.raise_error
let syscmdf_exn fmt = ksprintf syscmd_exn fmt

let testing ?(add_static_dirs=[]) ?authentication ?debug
    ?ssl ?ssl_dir ?(port=8080) kind =
  let runtime_root = "/tmp/testocsibase" in
  let static_dirs = sprintf "%s/static/" runtime_root :: add_static_dirs in
  let open Result in
  syscmdf_exn "rm -fr %s/static/ && mkdir -p %s/static/" runtime_root runtime_root;
  let open Out_channel in
  with_file (sprintf "%s/mime.types" runtime_root)
    ~f:(fun o -> output_string o (mime_types));
  with_file (sprintf "%s/ocsibase.conf" runtime_root)
    ~f:(fun o ->
      config ?authentication ?ssl ?ssl_dir ?debug ~static_dirs
        ~port ~runtime_root kind (output_string o));
  syscmdf_exn "cp _build/ocsibase/ocsibase.js %s/static/" runtime_root;
  syscmdf_exn "cp _build/ocsibase/ocsibase.css %s/static/" runtime_root;
  begin match kind with
  | `Ocsigen ->
    syscmdf_exn "LD_LIBRARY_PATH=_build/src/simple_pam/:$LD_LIBRARY_PATH \
                 ocsigenserver -c %s/ocsibase.conf" runtime_root;
  | `Static -> 
    syscmdf_exn "ocsibaseserver -c %s/ocsibase.conf" runtime_root;
  end;
  ()


let sysv_init_file
    ~path_to_binary 
    ?(config_file="/etc/ocsibase/ocsibase.conf")
    ?(pid_file="/tmp/ocsibase/pidfile")
    ?(stdout_stderr_file="/tmp/ocsibase/stdout_stderr")
    ?(log_dir="/tmp/ocsibase/log/")
    ?(command_pipe="/tmp/ocsibase/ocsigen_command")
    output_string =
  let centos_friendly_header =
    sprintf  "#! /bin/sh
#
# ocsibase
#
# chkconfig: 345 99 01
# description:  Starts and stops Ocsibase
#
# config: %s
#
### BEGIN INIT INFO
# Provides: ocsibase
# Required-Start:  $syslog $network postgresql
# Required-Stop:  $syslog $network postgresql
# Default-Start: 3 4 5
# Default-Stop: 0 1 6
# Short-Description:  Ocsibase HTTP server
# Description: Start and stop Ocsibase
### END INIT INFO

# This file has been auto-generated, do not edit directly please.\
 
" config_file in
  let begining =
    sprintf "
set -e
case \"$1\" in" in
  let check_running ~do_then ?do_else () =
    sprintf "
    if [ -r \"%s\" ] && read pid < \"%s\" && ps -p \"$pid\" > /dev/null 2>&1; then
%s
%s
    fi
"   pid_file pid_file do_then 
      Option.(value ~default:"" (map ~f:(sprintf "else\n%s") do_else))
  in
  let start_case =
    sprintf "
   start|force-start)\
%s
    mkdir -p %s %s
    nohup %s --verbose --pidfile %s -c %s \
        > %s 2>&1 &
    ;;
"
      (check_running () ~do_then:"   echo \"Ocsibase is already running\"\n\
                              \   exit 0")
      (Filename.dirname stdout_stderr_file) log_dir
      path_to_binary pid_file config_file
      stdout_stderr_file
  in
  let stop_case = 
    sprintf "
  stop)
    echo -n \"Stopping Ocsibase: \"
    %s
    rm -f %s
    echo \"Done.\"
    ;;
" 
      (check_running ()
         ~do_then:(sprintf
                     "    echo shutdown > %s" command_pipe)
         ~do_else:"    echo \"Ocsibase was not running\"")
      pid_file in
  let kill_case = 
    sprintf "
  force-stop)
    echo -n \"Killing Ocsibase: \"
    for pid in `cat %s`; do kill -9 $pid || true; done
    rm -f %s
    echo \"Done.\"
    ;;
" 
      pid_file pid_file in
  let reload_case = 
    sprintf "
  reload)
    echo -n \"Reloading Ocsibase: \"
    echo reload > %s
    echo \"Done.\"
    ;;
" command_pipe in
  let status_case =
    sprintf "
  status)
    echo -n \"Status of Ocsibase: \"
    if [ ! -r \"%s\" ]; then
      echo \"It is NOT running.\"
      exit 3
    fi
    if read pid < \"%s\" && ps -p \"$pid\" > /dev/null 2>&1; then
      echo \"It IS running (pid: $pid).\"
      exit 0
    else
      echo \"it is NOT running BUT %s exists.\"
      exit 1
    fi
    ;;
" pid_file pid_file pid_file
  in
  let esac =
    sprintf "
  *)
    echo \"Usage: $0 {start|stop|reload|status|force-stop}\" >&2
    exit 1
    ;;
esac

exit 0
" in
  output_string centos_friendly_header;
  output_string begining;
  output_string start_case;
  output_string stop_case;
  output_string kill_case;
  output_string reload_case;
  output_string status_case;
  output_string esac


let rpm_build ?(add_static_dirs=[]) ?(version="0.0") ?(release=1) ?ssl ?ssl_dir () =
  let () =
    match Unix.system 
      "test \"`ocamlfind list | grep batteries | grep 1.4 | wc -l`\" -eq 0"
    with
    | `Exited 1 -> 
      printf
        "ERR: It seems to be the wrong version of Batteries, \
          I won't continue:\n";
      printf "$ ocamlfind list | grep batteries\n%!";
      syscmd_exn "ocamlfind list | grep batteries";
      failwith "Batteries_version"
    | _ -> ()
  in
  let tmp_dir = "/tmp/ocsibase_rpmbuild" in
  let spec_file = sprintf "%s/SPECS/ocsibase.spec" tmp_dir in
  let binary_tmp = sprintf "%s/ocsibaseserver" (Unix.getcwd ()) in
  let binary_dir = "/usr/libexec/ocsibase" in
  let binary_target = sprintf "%s/ocsibaseserver" binary_dir in
  let conf_tmp = (sprintf "%s/ocsibase.conf" tmp_dir) in
  let conf_root = "/etc/ocsibase" in
  let conf_target = (sprintf "%s/ocsibase.conf" conf_root) in
  let mimes_tmp = (sprintf "%s/mime.types" tmp_dir) in
  let mimes_target = (sprintf "%s/mime.types" conf_root) in
  let sysv_tmp = sprintf "%s/ocsibase.init" tmp_dir in
  let sysv_target = "/etc/init.d/ocsibase" in
  let runtime_root = "/var/ocsibase" in
  let static_dir = "/var/ocsibase/static" in
  let log_dir = "/var/log/ocsibase" in

  let javascript_tmp =
    sprintf "%s/_build/ocsibase/ocsibase.js"  (Unix.getcwd ()) in
  let javascript_target =
    sprintf "%s/ocsibase.js" static_dir in

  let css_tmp =
    sprintf "%s/_build/ocsibase/ocsibase.css"  (Unix.getcwd ()) in
  let css_target =
    sprintf "%s/ocsibase.css" static_dir in
  
  let static_dirs = static_dir :: add_static_dirs in
  
  let open Out_channel in

  List.iter [ "BUILD"; "SPECS"; "RPMS"; "SOURCES"; "SRPMS" ]
    ~f:(fun dir ->
      syscmdf_exn "mkdir -p %s/%s" tmp_dir dir);

  with_file mimes_tmp
    ~f:(fun o -> output_string o (mime_types));
  
  with_file conf_tmp ~f:(fun o -> 
    config ~port:80 ~runtime_root ~conf_root ?ssl ?ssl_dir ~static_dirs
      ~log_root:log_dir `Static (output_string o));
  
  with_file sysv_tmp ~f:(fun o ->
    sysv_init_file
      ~path_to_binary:binary_target
      ~config_file:conf_target
      ~pid_file:(sprintf "%s/pidfile" runtime_root)
      ~stdout_stderr_file:(sprintf "%s/stdout_and_stderr" runtime_root)
      ~command_pipe:(sprintf "%s/ocsigen_command" runtime_root)
      ~log_dir
      (output_string o)
  );

  with_file spec_file ~f:(fun o ->
    fprintf o "%%define _topdir %s\n" tmp_dir;
    fprintf o "%%define name ocsibase\n";
    fprintf o "%%define release %d\n" release;
    fprintf o "%%define version %s\n" version;
    output_string o "
Name:   %{name}
Version: %{version}
Release: %{release}
Summary: Ocsibase web server and web app

Group:  Web Server
License: MIT
URL:    http://seb.mondet.org
#Source0:       
BuildRoot:      %(mktemp -ud %{_topdir}/%{name}-%{version}-%{release}-XXXXXX)\

Requires: libev openssl pcre sqlite zlib

%description
Ocsibase bundles an Ocsigen webserver with an embedded webapp.

%prep
# %setup -q
echo 'Preparing'

%build
echo 'Building'

%install
rm -rf $RPM_BUILD_ROOT
";
    fprintf o "mkdir -p $RPM_BUILD_ROOT/%s\n" binary_dir;
    fprintf o "mkdir -p $RPM_BUILD_ROOT/%s\n" conf_root;
    fprintf o "mkdir -p $RPM_BUILD_ROOT/%s\n" static_dir;
    fprintf o "mkdir -p $RPM_BUILD_ROOT/etc/init.d\n";
    fprintf o "cp %s $RPM_BUILD_ROOT/%s\n" binary_tmp binary_dir;
    fprintf o "cp %s $RPM_BUILD_ROOT/%s\n" conf_tmp conf_root;
    fprintf o "cp %s $RPM_BUILD_ROOT/%s\n" mimes_tmp conf_root;
    fprintf o "cp %s $RPM_BUILD_ROOT/%s\n" sysv_tmp sysv_target;
    fprintf o "cp %s $RPM_BUILD_ROOT/%s\n" javascript_tmp static_dir;
    fprintf o "cp %s $RPM_BUILD_ROOT/%s\n" css_tmp static_dir;
    output_string o "
%clean
rm -rf $RPM_BUILD_ROOT

%files
";
    output_string o "%defattr(0555,root,root,-)\n";
    fprintf o "%s/ocsibaseserver\n" binary_dir;
    fprintf o "%s\n" sysv_target;
    output_string o "%defattr(0444,root,root,-)\n";
    fprintf o "%s\n" conf_target;
    fprintf o "%s\n" mimes_target;
    fprintf o "%s\n" javascript_target;
    fprintf o "%s\n" css_target;
    fprintf o "%s/*\n" static_dir;
    output_string o "

%doc

%changelog

%post
# Register the service
/sbin/chkconfig --add ocsibase

%preun
if [ $1 = 0 ]; then
        /sbin/service ocsibase stop > /dev/null 2>&1
        /sbin/chkconfig --del ocsibase
fi
";
  );
  syscmd_exn (sprintf "rpmbuild -v -bb --clean %s" spec_file)

let () =
  match Array.to_list Sys.argv with
  | [] | _ :: [] | _ :: _ :: [] ->
    eprintf "Usage: ocsibase {test|static|rpm}\n"
  | exec :: cmd_args ->
    let port = ref 80 in 
    let rpm_version = ref "0.0" in
    let rpm_release = ref 1 in
    let ssl_cert, ssl_key = ref "", ref "" in
    let ssl_dir = ref None in
    let options = [
      (`run, "-port", Arg.Set_int port,
       sprintf "p\n\tPort number (default: %d), \
              the HTTPS port is !port - 80 + 443 ;)" !port);
      (`rpm, "-rpm-release", Arg.Set_int rpm_release,
       sprintf "n\n\tSet the RPM release number (default: %d)" !rpm_release);
      (`rpm, "-rpm-version", Arg.Set_string rpm_version,
       sprintf "n\n\tSet the RPM version string (default: %s)" !rpm_version);
      (`all, "-ssl", Arg.(Tuple [Set_string ssl_cert; Set_string ssl_key]),
       sprintf "<cert> <key>\n\tSet an SSL certificate and a key filenames.");
      (`all, "-ssl-dir", Arg.String (fun s -> ssl_dir := Some s),
       sprintf "<dir>\n\tDirectory of the SSL certificates and keys
        (default is the same as the one used for ocsibase.conf)");
    ] in
    let anon_args = ref [] in
    let anon s = anon_args := s :: !anon_args in
    let usage = 
      sprintf "Usage:" in
    let cmdline = Array.of_list (cmd_args) in
    let cmd = List.hd_exn cmd_args in
    begin 
      try
        let actual_options =
          List.filter_map options (function
          | (`all, x,y,z) -> Some (x,y,z)
          | (`run, x,y,z) when cmd = "test" || cmd = "static" -> Some (x,y,z)
          | (`rpm, x,y,z) when cmd = "rpm" -> Some (x,y,z)
          | _ -> None) in
        Arg.parse_argv cmdline actual_options anon usage;
      with
      | Arg.Bad b -> eprintf "Error wrong argument %s\n" b; exit 1
      | Arg.Help h -> eprintf "Help: %s\n" h; exit 0
    end;

    let ssl = match !ssl_cert, !ssl_key with
      | "", "" -> None
      | c, k -> Some (c,k) in
    begin match cmd with
    | "test"  -> testing ~port:!port ?ssl ?ssl_dir:!ssl_dir ~debug:true `Ocsigen
    | "static" -> testing ~port:!port ?ssl ?ssl_dir:!ssl_dir `Static
    | "rpm" -> rpm_build ~release:!rpm_release ?ssl ?ssl_dir:!ssl_dir ()
    | "sysv" -> sysv_init_file ~path_to_binary:"/bin/ocsibase" print_string
    | not_found -> eprintf "Unknown command: %s.\n" not_found
    end
