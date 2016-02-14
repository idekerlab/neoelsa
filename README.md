elsa
=====

An OTP application

Build
-----

    $ rebar3 compile

A service is your program's interface to the world. It's an abstraction and cannot fulfill requests unless instances of your service, which provide the actual implementation, exist. A service is uniquely identified by its name and version. It's very important to understand that a program IS its interface, and that different versions of a program with the same name are essentially different programs if their interfaces are different, and Elsa views them as such. For this reason, you should only make a version change (i.e. v1 to v2) when your interface changes. It's also good practice to allow Elsa to manage your versioning and forgo building a versioned urls into your service.


An instance is the running implementation of your service. When you register an instance under your service (remember that a service is a name AND a version) it should implement exactly the interface described by that service. If it does not, it will fail to fulfill requests and Elsa will disconnect the instance from the instance group.

When you register an instance, you specify the instance's capacity. The capacity of an instance is the number of parallel connections it can handle. If Elsa opens a connection to your instance within this threshold and the connection fails, Elsa will disconnect your instance from the instance group. An instance can send the string "infinity" instead of a numeric quantity, in which case Elsa assumes your instance can handle an infinite number of simultanious connections.

It is always better to have multiple instances registered for your service. This ensures a better change of availability to your users. The design of Elsa allows for heavy load, but if you only use one instance and that instance fails, you're service will become unavailable. 
