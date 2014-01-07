

#Spec: a new framework for describing user intefaces


##1\. Introduction


Writing user interfaces is notoriously a tedious tasks\.It often requires time and a clear understanding of the separation of concerns\.Indeed most of the frameworks mix domain applicative models with widget models\.

*Spec* is a framework for describing user interfaces\. It allows the separation of concerns between the different part of the user interface as expressed in the MVP pattern\.

This article goal is to provide an overview of the *Spec* functionalities\.The general purpose of *Spec* will be explained through several examples among multiple sections\.

First the 3 pillars of *Spec* will be explained\.Second the API of the *Spec* basic models is detailled as well as how to understand the meta information attached to this API\.The third section covers the two dynamic aspects of *Spec*\.The final section is dedicated to the creation of your own *Spec* model\.

##2\.  The heart of Spec


Spec is build around three axes borrowed form the MVP pattern\.Those axes are materialize as three methods: `initializeWidgets`, `initializePresenter` , and `defaultSpec`\.


###2\.1\.  `initializeWidgets` the Presenter




###2\.2\.  `initializePresenter` the Interactor	




###2\.3\.  `defaultSpec` the View


##3\.  Where to find what I want


##4\.  Spec the Dynamic



###4\.1\.  Dynamic add and removal of subwidgets



###4\.2\.  Dynamic modification of the layout


##5\.  Writing my own Spec



###5\.1\.  The Model



###5\.2\.  The Adapter



###5\.3\.  The Bindings
