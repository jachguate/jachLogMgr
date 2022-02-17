# jachLog

A pure pascal, flexible, extensible and lightweight library to add logging capabilities to your Delphi applications. The library supports multithreaded applications and, in the future, may be itself multithreaded, to minimize even more the impact it have on the performance on mission critical applications.

The main logging class provide methods to add log records in a simple, yet powerful manner.

Main features:

* The entire log can be switched on and off.
* Multiple logging categories can be switched on and off individually
* Multiple logging levels to allow 
* Multiple logging destinations
	- Disk
	- Console
	- Local or remote Syslog (future, not yet implemented)
		+ Indy based (future, not yet implemented)
		+ ICS based (future, not yet implemented)
	- Database (future base class, not yet implemented)
	- Android standard log (future, not yet implemented)

Delphi versions supported: 10 and Up. (Las version tested is Delphi 11)

The use of the library is intuitive and you will add it to your project and setup a simple log to disk in less than 5 minutes.

Take a look at the demos folder to learn how the library is used.

As for now, the author plans to release a couple of tutorial videos on youtube and Embarcadero developer conferences in English and Spanish.

Contact the developer via mail on *jachguate at gmail dot com*.
