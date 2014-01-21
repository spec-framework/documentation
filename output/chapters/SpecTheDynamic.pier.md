

##1\.  Spec the Dynamic
<a name="sec_spec_the_dynamic"></a>

Having an user interface with a well known number of sub widgets and a static layout is not always sufficient\. 
A user interface is often more than just that, for example here are two situations where more is needed: 
First, when the layout of the user interface needs to be changed at runtime to match the execution context of the software\.
Second, sub widgets are added or removed at runtime and therefore the programmer needs to be able to parametrize those new sub widgets on the fly\.


*Spec* also provides support for such dynamic user interfaces\.
In this section we show how to use 
*Spec* in these situations\.


To be able to compose dynamic user interfaces at run time, a new method has been introduced\.
This method is 
`assign:to:` and take a model instance as a first argument, and a unique symbol as a second argument\.
This way composing dynamic model is as simple as composing any other user interface\.


First, we talk about making dynamic modifications of the layout of widgets, and second we discuss the dynamic adding and removing of subwidgets\.
Third and last we show how the dynamic features can be used to quickly prototype a user interface\.



###1\.1\.  Dynamic modification of the layout


Changing the layout of widgets at runtime is straightforward, as we will see here\.
Such changes basically consist of three steps:


1.  creating the new layout,
2.  setting a flag to prohibit the creation of a new UI element \(and instead reuse the existing one\),
3.  building the UI again with the newly created layout\.


The code in 
[1\.1](#rebuildDynamically) is an example of rebuilding a widget with a new layout\.
First, a helper method is used to obtain a 
`SpecLayout` object that determines the new layout\.
Second, the 
`needRebuild` flag is set to 
`false` such that the existing UI element is reused\.
This leads to the replacement of the content of the current container instead of just instantiating a new UI element\.
Third, the rebuilding of the user interface is performed\.




<a name="rebuildDynamically"></a>**Rebuild a widget at run time**


    rebuildWithNewLayout
    	| newLayout |
    
    	newLayout := self newLayoutCreatedDynamically.
    	self needRebuild: false. "tells the interpreter to keep my current UI element"
    	self buildWithSpecLayout: newLayout. "rebuilds me with the new layout"



One widget can also keep the UI elements of its sub widgets which do not need to be rebuilt\.
The message 
`needRebuild: false` needs to be sent to any of those sub widgets\.
For example, if a model comprising a 
*button* and a 
*list* just wants to rearrange the position of these UI elements, there is no need to rebuild them, i\.e\. instantiate new UI elements for them\.
To prevent this, the message 
`needRebuild: false` should be send to them, as shown in the example 
[1\.2](#ex_needRebuild)\.




<a name="ex_needRebuild"></a>**How to need rebuild sub widgets**


    rebuildWithNewLayout
    	| newLayout |
    
    	newLayout := self newLayoutCreatedDynamically.
    	self needRebuild: false.
    	theButton needRebuild: false.
    	theList needRebuild: false.
    	self buildWithSpecLayout: newLayout.




###1\.2\.  Dynamic adding and removal of subwidgets


If a user interface needs a varying number of subwidgets, the amount of which cannot be established at compilation time, then another approach is needed\.
In this scenario, 
`DynamicComposableModel` is the model that needs to be subclassed, as this class provides support for the required kind of dynamic behavior\.


When using 
`DynamicComposableModel`, the instantiation of the sub widgets is a bit different from normal use\.
In the 
`instantiateWidgets` method, instead of instantiating each widget separately, 
`instantiateModels:` should be used to instantiate them\.
This method takes as argument an array of pairs, where each pair is composed of the unique name of the widget as key, and the name of the widget class as value\.
This allows for a widget to be accessed by sending a message whose selector is the widget name to the model\.


By example, if a widget named 
`button` is created, then this widget can be accessed by calling 
`self button` as shown in the example 
[1\.3](#ex_dynamic_creation)\.




<a name="ex_dynamic_creation"></a>**Dynamic creation of a widget**


    self instantiateModels: #( button ButtonModel ).
    	self button label: 'Click me'.




###1\.3\.  Examples: Prototyping a UI


Thanks to the capability of 
*Spec* to dynamically instantiate widgets, it is also possible to prototype a user interface from within any workspace\.
The following examples show how 
*Spec* can be used to prototype quickly a user interace\.


The first example explains how to design by prototyping a user interface\.
The second example introduce the composition of dynamic models\.



####1\.3\.1\.  Designing a pop up


This example shows how to easily and quickly design a popup window asking for an input\.


First we create a simple model with two sub widgets, a label and a text field, as shown by the snippet 
[1\.4](#ex_widget_creation)\.




<a name="ex_widget_creation"></a>**Create a widget**


    view := DynamicComposableModel new
    	instantiateModels: #(label LabelModel text TextInputFieldModel);
    	yourself.



We can then specify the title and the initial size of the widget, adding the code in 
[1\.5](#ex_set_title)\.




<a name="ex_set_title"></a>**Specify the title and the initial size**


    view extent: 300@90;
    	title: 'Choose your project'.



Then we specify the UI element layout\.
It will be only one row with the label and the text field\.
The snippet 
[1\.6](#ex_layout) shows the layout definition\.




<a name="ex_layout"></a>**Define the layout**


    layout := SpecLayout composed
    	newRow: [ :r | r add: #label width: 75; add: #text ];
    	yourself.



To have a first idea of the resulting UI, we can already open it as follows: 
`view openWithSpecLayout: layout`\.
This UI however does not have any sub widget state or behavior so there is not much to see or do at this point\.


The next step is to set up the sub widget state and behavior\.
We set the text of the label as well as the ghost text of the textfield\.
We also specify here that the text field should automatically accept the text on each keystroke, such that it does not show the yellow 'edited' triangle on the top right\.
This is shown in the code in 
[1\.7](#ex_setup_subwidget)\.




<a name="ex_setup_subwidget"></a>**Setting up the sub widgets**


    view label text: 'Packages:'.
    
    view text
    	autoAccept: true;
    	entryCompletion: nil;
    	ghostText: '.*'.



Opening the UI again \(
`view openWithSpecLayout: layout`\) now shows the text of the label and the ghost text of the text field\.


As we want the widget to be a popup with a single button 
*Ok*, the toolbar to use should be defined explicitly \(the default toolbar has an 
*Ok* button and a 
*Cancel* button\)\.
We also set the toolbar action for when 
*Ok* is clicked: the current text of the text field will be saved in the instance variable 
*regex*\.
The code in 
[1\.8](#ex_toolbar) shows how to do it\.




<a name="ex_toolbar"></a>**Instantiating the toolbar**


    toolbar := OkToolbar new
    	okAction: [ regex := view text text ];
    	yourself.



We can also add a shortcut to the text field on 
*Enter* to simulate the click on 
*Ok*\.
The code 
[1\.9](#ex_shortcut) illustrates how to set up such a shortcut\.




<a name="ex_shortcut"></a>**Add a shortcut**


    view text 
    	bindKeyCombination: Character cr asKeyCombination 
    	toAction: [ toolbar triggerOkAction ].



This completes the specification of the UI\.
As a final step, when opening it we pass it the toolbar and configure it to be centered in the Pharo window and modal\.
The code in 
[1\.10](#ex_final) shows the final version of the code




<a name="ex_final"></a>**Open the widget**


    view := DynamicComposableModel new
    	instantiateModels: #(label LabelModel text TextInputFieldModel);
    	extent: 300@90;
    	title: 'Choose your project'
    	yourself.
    	
    view label text: 'Packages:'.
    
    layout := SpecLayout composed
    	newRow: [ :r | r add: #label width: 75; add: #text ];
    	yourself.
    
    view text
    	autoAccept: true;
    	entryCompletion: nil;
    	ghostText: '.*'.
    	
    toolbar := OkToolbar new
    	okAction: [ regex := view text text ];
    	yourself.
    	
    view text 
    	bindKeyCombination: Character cr asKeyCombination 
    	toAction: [ toolbar triggerOkAction ].
    	
    (view openDialogWithSpecLayout: layout)
    	toolbar: toolbar;
    	centered;
    	modalRelativeTo: World.



The result can be seen in Figure 
[1\.1](#fig_popup)\.


<a name="fig\_popup"></a>![fig\_popup](figures/Popup.png "Prototype of a popup")


####1\.3\.2\.  Composing dynamic models


This exemple shows in three parts how to buid a simple code browser\.


First a simple list widget is created displaying all the subclasses of AstractWidgetModel\.




<a name="ex_dyn_list"></a>**Defining a list widget**


    m := DynamicComposableModel new.
    m instantiateModels: #( list ListModel ).
    m list items: (AbstractWidgetModel allSubclasses sorted: [:a :b | a name < b name ]).
    m layout: (SpecLayout composed
    	add: #list;
    	yourself).
    m openWithSpec.



Then the list widget is reused to build a viewer widget displaying the protocol methods of the selected class\.




<a name="ex_dyn_protocols"></a>**Definition of a protocol methods viewer**


    m2 := DynamicComposableModel new.
    m2 assign: m to: #classes.
    m2 instantiateModels: #( methods ListModel ).
    m list whenSelectedItemChanged: [ :item | 
    	item 
    		ifNil: [ m2 methods: #() ]
    		ifNotNil: [ m2 methods items: ((item selectorsInProtocol: 'protocol') sorted) ] ].
    m2 layout: (SpecLayout composed
    	newRow: [ :r | r add: #classes; add: #methods ];
    	yourself).
    m2 openWithSpec.



Finally the last widget is defined with the viewer previously created\.
In addition, a text zone is edited to show the selected method source code\.




<a name="ex_dyn_browser"></a>**Definition of a Protocol Browser**


    m3 := DynamicComposableModel new.
    m3 assign: m2 to: #top.
    m3 instantiateModels: #( text TextModel ).
    m2 methods whenSelectedItemChanged: [ :selector | 
    	selector ifNil: [ m3 text text: '' ] ifNotNil: [ m3 text text: (m list selectedItem >> selector ) sourceCode ] ].
    m3 layout: (SpecLayout composed
    	newColumn: [ :c | c add: #top; add: #text ];
    	yourself).
    	
    m3 openWithSpec.
    m3 title: 'Protocol browser'



The final result looks like the Figure 
[1\.2](#ex_browser)\.


<a name="ex\_browser"></a>![ex\_browser](figures/Protocol_Browser.png "Prototype of Protocol Browser")