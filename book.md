

#Spec: a new framework for describing user intefaces 


##Introduction 



The following article will introduce you how to use  `Spec` through several examples\.  The general purpose of  `Spec` will be explained then will follow a presentation of every  `Spec` widgets and how to use them\. This paper will be concluded by  `n` examples about how to compose basic widgets in order to build complete user interfaces\. 

## The heart of Spec 
<a name="sec:heart_of_spec"></a>

Spec is built around three axes borrowed from the MVP pattern\. Those axes are materialized as three methods:  `initializeWidgets`,  `initializePresenter`, and  `defaultSpec`\. We first detail some necessary terminology before discussing each of these in detail\. 


To avoid possible misundertandings due to terminology issues because of overloaded meanings, we first define four terms: 
<dl><dt>UI Element
</dt><dd>an interactive graphical element displayed as part of the Graphical User Interface.</dd><dt>UI Model
</dt><dd>an object that contains the state and behavior of one or several UI elements.</dd><dt>Widget
</dt><dd>the union of a UI Element and its UI model.</dd><dt>Basic widgets
</dt><dd>low level widgets like a list, a button, etc. They are not composed of other widgets.</dd></dl>



####  `initializeWidgets` the widgets 


This takes care of the configuration of the different widgets themselves, without the interaction\. 



    Note: Ben I think you might want to have a look at the mvp paper please.




####  `initializePresenter` the Presenter 

<a name="pattern"></a>**Example of initializeWidgets**


    initializeWidgets
    
    	button := self newButton.
    
    	button label: 'I am a button'.
    	
    	self focusOrder
    		add: button



Note that this method is mandatory\. Each subclass need to override it\. 


#### Widget instantiation 


The instantiation of a widget can be done in two ways\. First if it is a basic widget \(like a button or a list\) which is instantiated, then the framework provides accesors for them\. The format of this accessors is  `new[Widget]`\. By example  `newList`,  `newText`, etc\. The complete list of available widget creation methods can be found in the class  *ComposableModel* in the protocol  *widgets*\. 

Second if one wants to reuse any subclass of  *ComposableModel* the widget needs to be initialized using the  `instantiate:` method\. The example  [1\.2\. ](#use_of_instantiate) shows how to instantiate a  *MessageBrowser*\. 



<a name="use_of_instantiate"></a>**How to reuse a MessageBrowser widget**


    widget := self instantiate: MessageBrowser.




###  *initializePresenter* the Presenter  <sub>the MVP Interactor</sub>


This takes care of the interactions between the different widgets\. This method specifies the flow of exectution linking the sub widgets all together\. All the interaction between one widget and another is specified here\. 

Usually this method is composed of actions to perform when a specific event is received\. Based on value holders,  **Spec** event mechanism rely on the value holder announcements\. Value holders provide a single method  `whenChangedDo:` to register a block to perform on change\. The propagation of those events build the whole interaction flow\. 

Moreover the basic widgets provide a full set of registration methods\. The whole API is described in the section  [¿?\. ](#sec:where_to_find_what_I_want)\. 




    If a programmer wants his or her widgets to be reused,
    they should provide a comprehensive API.



Not that this method is optional\. 


###  *defaultSpec* the View  <sub>the MVP Presenter</sub>


This takes care of the layout of the different widgets\. This class side method is used to specify the position of each sub widgets\. It also specifies how a widget reacts when the window is resized\. 

Multiple layouts can be described\. Then when the widget is built a specific layout can be specified\. 

This method is on class side because usually all the instances of a same user interface have the same layout\. But the lookup for the spec method to use starts on instace side\. This way a class can have a more specific layout depending of the instance state\. 


#### Pragmas 


As previously said, multiple views can be described for the same user interface\. In order to retrieve the correct method to apply, the spec methods are flagged with a pragma\. 

The pragma can be  `<spec: default>` for the view to use by default, or  `<spec>` for the other views\. 


#### Examples 

<a name="layout_basic_example@"></a>
The simpliest example is to just render one widget\. The example  [1\.3\. ](#ex:layout1) presents such a layout\. 



<a name="ex:layout1"></a>**Layout with only one widget**


    ^ SpecLayout composed
    	add: #myWidget;
    	yourself



The symbol  `myWidget` refers to an instance side method returning a widget\. Note that by default, a widget will take all the space available\. 

---

<a name="layout_rows_and_column_layout@"></a>
Often user interfaces can be describes in rows and columns\. The example  [1\.4\. ](#ex:layout_rows) sows how to build a row of widgets\. 



<a name="ex:layout_rows"></a>**Row of widgets**


    ^ SpecLayout composed
    	newRow: [ :row |
    		row
    			add: #myFirstWidget;
    			add: #mySecondWidget
    	];
    	yourself



## Where to find what I want 


## Spec the Dynamic 



### Dynamic add and removal of subwidgets 



### Dynamic modification of the layout 


## Writing my own Spec 



### The Model 



### The Adapter 



### The Bindings 
