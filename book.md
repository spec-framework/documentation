

#Spec: a new framework for describing user intefaces 


##Introduction 



The following article will introduce you how to use  `Spec` through several examples\.  The general purpose of  `Spec` will be explained then will follow a presentation of every  `Spec` widgets and how to use them\. This paper will be concluded by  `n` examples about how to compose basic widgets in order to build complete user interfaces\. 

## The heart of Spec 


Spec is built around three axes borrowed from the MVP pattern\. Those axes are materialized as three methods:  `initializeWidgets`,  `initializePresenter`, and  `defaultSpec`\. We first detail some necessary terminology before discussing each of these in detail\. 


To avoid possible misundertandings due to terminology issues because of overloaded meanings, we first define three terms: 


-   *UI Element:* an interactive graphical element displayed as part of the Graphical User Interface\. 
-   *UI Model:* an object that contains the state and behavior of one or several UI elements\. 
-   *Widget:* the union of a UI Element and its UI model\. 

&nbsp;


####  `initializeWidgets` the widgets 


This takes care of the configuration of the different widgets themselves, without the interaction\. 



    Note: Ben I think you might want to have a look at the mvp paper please.




####  `initializePresenter` the Presenter 


This takes care of the interactions between the different widgets\. 


####  `defaultSpec` the View 


This takes care of the layout of the different widgets\. 

This is a class side method, because it actually is just an accessor for data that is shared by all the instances\. 

## Where to find what I want 


## Spec the Dynamic 



### Dynamic add and removal of subwidgets 



### Dynamic modification of the layout 


## Writing my own Spec 



### The Model 



### The Adapter 



### The Bindings 
