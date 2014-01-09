

#Spec: a new framework for describing user intefaces 


##Introduction 



The following article will introduce you how to use  `Spec` through several examples\.  The general purpose of  `Spec` will be explained then will follow a presentation of every  `Spec` widgets and how to use them\. This paper will be concluded by  `n` examples about how to compose basic widgets in order to build complete user interfaces\. 

## The heart of Spec 
<a name="heart_of_spec"></a>

Spec is built around three axes borrowed from the MVP pattern\. Those axes are materialized as three methods:  `initializeWidgets`,  `initializePresenter`, and  `defaultSpec`\. We first detail some necessary terminology before discussing each of these in detail\. 


To avoid possible misundertandings due to terminology issues because of overloaded meanings, we first define three terms: 


-   *UI Element:* an interactive graphical element displayed as part of the Graphical User Interface\. 
-   *UI Model:* an object that contains the state and behavior of one or several UI elements\. 
-   *Widget:* the union of a UI Element and its UI model\. 

&nbsp;


###  *initializeWidgets* the widgets  <sub>the MVP View</sub>


This takes care of the configuration of the different widgets themselves, without the interaction\. It is used to instantiate the subwidgets and to specify their default values and general behavior\. This focus in this method is to specify how the widgets will look like\. 

In general  `initializeWidgets` should follow the pattern: 


-  widgets instantiation 
-  widgets specification 
-  set the order of focus 

&nbsp;

The code  [1\.1\. ](#pattern) is an example of an  `initializeWidgets` method\. 



<a name="pattern"></a>**
Example of initializeWidgets **


    initializeWidgets
    
    	button := self newButton.
    
    	button label: 'I am a button'.
    	
    	self focusOrder
    		add: button




#### Widget instantiation 


The instantiation of a widget can be done in two ways\. First if it is a basic widget \(like a button or a list\) which is instantiated, then the framework provides accesors for them\. The format of this accessors is  `new[Widget]`\. By example  `newList`,  `newText`, etc\. The complete list of available widget creation methods can be found in the class  *ComposableModel* in the protocol  *widgets*\. 

Second if one wants to reuse any subclass of  *ComposableModel* the widget needs to be initialized using the  `instantiate:` method\. The example  [1\.2\. ](#use_of_instantiate) shows how to instantiate a  *MessageBrowser*\. 



<a name="use_of_instantiate"></a>**
How to reuse a MessageBrowser widget **


    widget := self instantiate: MessageBrowser.




###  *initializePresenter* the Presenter  <sub>the MVP Interactor</sub>


This takes care of the interactions between the different widgets\. This method specifies the flow of exectution linking the sub widgets all together\. All the interaction between one widget and another is specified here\. 


###  *defaultSpec* the View  <sub>the MVP Presenter</sub>


This takes care of the layout of the different widgets\. This class side method is used to specify the position of each sub widgets\. It also specifies how a widget reacts when the window is resized\. 

Multiple layouts can be described\. Then when the widget is built a specific layout can be specified\. 

This method is on class side because usually all the instances of a same user interface have the same layout\. But the lookup for the spec method to use starts on instace side\. This way a class can have a more specific layout depending of the instance state\. 

## Where to find what I want 


## Spec the Dynamic 



### Dynamic add and removal of subwidgets 



### Dynamic modification of the layout 


## Writing my own Spec 



### The Model 



### The Adapter 



### The Bindings 
