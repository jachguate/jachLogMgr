# jachLog

A pure pascal, flexible, extensible and lightweight library to add logging capabilities to your Delphi applications. The library supports multithreaded applications and is itself multithreaded to minimize the impact writing the log may have on the performance on mission critical applications.

The main logging class provide methods to add log records in a simple, yet powerful manner.

## Features

The main features of the library are:

* The entire log can be switched on and off.
* Multiple logging topics can be switched on and off individually.
* Multiple logging levels to allow filtering out messages by topic and severity level.
* Multiple logging destinations
	- Disk (auto rotate by size limit)
	- Console
	- Visual rich edit component (VCL and FMX) (Coming in v 2.1)
	- Local or remote Syslog 
		+ Indy based (Coming in v 2.2)
		+ ICS based (Only if required by the community)
		+ Based on other TCP/UDP library (Only if required by the community)
	- Windows event log (Coming on v 2.5)
	- Android standard log (Coming on v 2.6)
	- Linux standard log (Coming on v 3.0+)
	- Database base class (Coming on v 3.0+)

## Delphi versions

Delphi versions supported: 10 and Up. (Last version tested is Delphi 11). If you successfully use it on an earlier version, please let the author know that.

## Documentation

The official documentation resides in the project wiki at github: [jachLog documentatrion](https://github.com/jachguate/jachLogMgr/wiki).

## Getting started

It takes 5 minutes to download the code and start using the library, read the [get started guide](https://github.com/jachguate/jachLogMgr/wiki/GetStartedWithJachLog)

## Demos

Take a look at the demos folder to learn how the library is used. To learn more abut them, read the [Documentation of the jachLog demo applications](https://github.com/jachguate/jachLogMgr/wiki/Demos)

## Contact

Contact the developer via mail on *jachguate at gmail dot com*.
