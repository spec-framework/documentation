

#Spec: a new framework for describing user interfaces


##1\. Introduction


Writing user interfaces is notoriously a tedious task\.It often requires time and a clear understanding of the separation of concerns\. Indeed most of the frameworks mix domain applicative models with widget models\.

*Spec* is a framework for describing user interfaces\. It allows the separation of concerns between the different part of the user interface as expressed in the MVP pattern\.*Spec* emphasis the reuse of the widgets as well as there customization\.

This article goal is to provide an overview of the *Spec* functionalities\. The general purpose of *Spec* will be explained through several examples among sections\.

First the 3 pillars of *Spec* will be explained\. Second we expose how *Spec* reuse subwidgets and how to customize widgets to make them fit the user needs\.Third the API of the *Spec* basic models is detailled as well as how to understand the meta information attached to this API\. The fourth section covers the two dynamic aspects of *Spec*\. The final section is dedicated to the creation of your own *Spec* model\.

##2\.  The heart of Spec
<a name="sec_heart_of_spec"></a>

Spec is built around three axes borrowed from the MVP pattern\.Those axes are materialized as three methods: `initializeWidgets`, `initializePresenter`, and `defaultSpec`\.We first detail some necessary terminology before discussing each of these in detail\.


To avoid possible misundertandings due to terminology issues because of overloaded meanings, we first define four terms:
<dl><dt>UI Element
</dt><dd>an interactive graphical element displayed as part of the Graphical User Interface.</dd><dt>UI Model
</dt><dd>an object that contains the state and behavior of one or several UI elements.</dd><dt>Widget
</dt><dd>the union of a UI Element and its UI model.</dd><dt>Basic widgets
</dt><dd>low level widgets like a list, a button, etc. They are not composed of other widgets.</dd></dl>



###2\.1\.  *initializeWidgets* the widgets <sub>the MVP View</sub>


This takes care of the configuration of the different widgets themselves, without the interaction\.It is used to instantiate the subwidgets and to specify their default values and general behavior\.This focus in this method is to specify how the widgets will look like\.

In general `initializeWidgets` should follow the pattern:


-  widgets instantiation
-  widgets specification
-  set the order of focus

&nbsp;

The code [1\.1\. ](#pattern) is an example of an `initializeWidgets` method\.



<a name="pattern"></a>**Example of initializeWidgets**


    initializeWidgets
    
    	theButton := self newButton.
    	theList := self newList.
    
    	theButton label: 'I am a button'.
    	
    	self focusOrder
    		add: theButton;
    		add: theList.






    Specifying this method is mandatory, as without it the UI would have no widgets.




####2\.1\.1\.  Widget instantiation


The instantiation of a widget can be done in two ways: through the use of an creation method or through the use of the `instantiate:` method\.Considering the first option, the framework provides unary messages for the creation of all basic widgets\.The format of these messages is `new[Widget]`, for example `newButton` creates a button widget, and `newList` creates a list widget, as we have seen above\.The complete list of available widget creation methods can be found in the class *ComposableModel* in the protocol *widgets*\.Considering the second option, to reuse any composite widgets, i\.e\. a subclass of *ComposableModel*, the widget needs to be initialized using the `instantiate:` method\.For example, to reuse a *MessageBrowser*  widget, the code is ` self instantiate: MessageBrowser.`


###2\.2\.  The *initializePresenter* method <sub>\(the MVP Interactor\)</sub>


This method takes care of the interactions between the different widgets\.By linking the behavior of the different widgets it specifies the overall presentation, i\.e\. how the overall UI responds to interactions by the user\.

Usually this method consists of specifications of actions to perform when a certain event is received by a widget\.From the propagation of those events the whole interaction flow of the UI emerges\.In  **Spec**, the different ui models are contained in value holders, and the event mechanism relies on the announcements of these value holders to manage the interactions between widgets\.Value holders provide a single method `whenChangedDo:` that is used to register a block to perform on change\.In addition to this primitive  `whenChangedDo:` method, the basic widgets provide more specific hooks, e\.g\. when an item in a list is selected or deselected\.

The example [2\.2](#ex_button) shows how to use one of registration methods from the list widget to change the label of the button according to the list selection\.



<a name="ex_button"></a>**How to change a button label according to a list selection**


    theList whenSelectedItemChanged: [ :item | 
    	item 
    		ifNil: [ button text: 'No selected item' ]
    		ifNotNil: [ button text: 'An item is selected']]



The whole event API of the basic widgets is described in the section [¿?](#sec_where_to_find_what_I_want)\.




    If a programmer wants his or her widgets to be reused,
    they should provide a comprehensive API.






    This method is optional. Without it, the different widgets in the UI will simply not respond to changes in each others' state.




###2\.3\.  the *layouting* method <sub>\(the MVP Presenter\)</sub>


This method specifies the layout of the different widgets in the UI\.It also specifies how a widget reacts when the window is resized\.

For the same UI multiple layouts can be described, and when the UI is built a specific layout can be specified\.If no specific layout is given, the layouting method returned by the lookup mechanism will be used\.

The lookup mechanism search on class side in the whole class hierarchy for a method with the pragma *<spec: \#default>*\.If multiple exists, the first one found will be used\.If none is found and only one method has the pragma *<spec>*, this method is used\.Otherwise an error is raised\.

This method is on class side because it returns a value that usually is the same for all the instances\.Put differently, usually all the instances of the same user interface have the same layout and hence this can be considered as being a class\-side accessor for a class variable\.Note that the lookup for the spec method to use starts on instance side, which allows a UI to have a more specific layout depending on the state of the instance\.

The simpliest example of such a method is laying out just one widget\.The example [2\.3](#fig:ex_layout1) presents such a layout\.It returns a layout in which just one widget is added: the widget contained in `theList` instance variable\.



<a name="fig:ex_layout1"></a>**Layout with only one widget**


    ^ SpecLayout composed
    	add: #theList;
    	yourself



The symbol `theList` refers to an instance side method returning a widget\.This is because as instance variables are private, the layout class needs to use an accessor to obtain it when building the UI\.Note that by default, a widget will take all the space available\.

As said above, multiple views can be described for the same user interface\.In order to retrieve the correct method to apply, these methods need to be flagged with a pragma\.The pragma can be either `<spec: default>` for the view to use by default, or `<spec>` for the other views\.




    Specifying this method is mandatory, as without it the UI would show no widgets to the user.





####2\.3\.1\.  Layout Examples


As layouts can become quite complex, this section provides a list of examples of the construction of layouts\.First two examples are given of the use of [rows and columns](#layout_rows_and_column_layout)\.This is followed by two examples that explain how to set a [fixed size](#layout_set_size_pixels) for rows and columns\.Next is an example that explains how to specify a widget [proportionally](#layout_percentage)\.The last example presents the [expert](#layout_expert) mode in case everything else fails\.To conclude, this section ends with a little [explanation](#layout_specify_layout) of how to specify which view to use and where to find the complete API\.
<a name="layout_rows_and_column_layout"></a>
Often the layout of user interfaces can be described in rows and columns, and **Spec** provides for an easy way to specify such layouts\.The example [2\.4](#fig:ex_layout_row) shows how to build a row of widgets\.



<a name="fig:ex_layout_row"></a>**Row of widgets**


    ^ SpecLayout composed
    	newRow: [ :row |
    		row
    			add: #theList;
    			add: #theButton
    	];
    	yourself



Having the widgets rendered as a column is similar, as shown in the example [2\.5](#fig:ex_layout_column)



<a name="fig:ex_layout_column"></a>**Column of widgets**


    ^ SpecLayout composed
    	newColumn: [ :column |
    		column
    			add: #theList;
    			add: #theButton
    	];
    	yourself




Then rows and columns can be combined to build more complex layouts\.The example [2\.6](#ex_three_columns) shows how to create a 3 columns layout with three buttons in each column\.This example also introduce `addSplitter` which is used to add a splitter between the element added before it and the element added after\.



<a name="ex_three_columns"></a>**3 columns layout**


    ^ SpecLayout composed
    	newRow: [ :row |
    		row
    			newColumn: [ :column | 
    				 column
    				 	add: #button1;
    					add: #button2;
    					add: #button3 
    			];
    			addSplitter;
    			newColumn: [ :column | 
    				 column
    				 	add: #button4;
    					add: #button5;
    					add: #button6 
    			];
    			addSplitter;
    			newColumn: [ :column | 
    				 column
    				 	add: #button7;
    					add: #button8;
    					add: #button9 
    			];
    	]




---

<a name="layout_set_size_pixels"></a>
The height of rows as well as the width of columns can be specified, to prevent them to take all the available space\.The example [2\.7](#fig:ex_row_height) shows how to specify the height of a row in pixels while the example [2\.8](#fig:ex_column_width) how to specify the column width\.



<a name="fig:ex_row_height"></a>**Row of 30 pixels**


    ^ SpecLayout composed
    	newRow: [ :row |
    		row
    			add: #theList;
    			add: #theButton
    	] height: 30;
    	yourself





<a name="fig:ex_column_width"></a>**Column of 30 pixels**


    ^ SpecLayout composed
    	newColumn: [ :column |
    		column
    			add: #theList;
    			add: #theButton
    	] width: 30;
    	yourself



Note that it is generally considered a bad habit to hardcode the size of the widgets\.Methods are available on *ComposableModel* providing sensible default sizes, like the width of a button\.When specifying custom widget sizes, care should be taken to take in account the current font size\.


---

<a name="layout_percentage"></a>
It is also possible to specify the percentage of the container, e\.g\. the window, that a widget should occupy\.As a result of this, the widget size will change accordingly when the container is resized\.To do so, the proportional position of the four sides of a widget can be specified, as shown in the example [2\.9](#ex_layout_proportional)\.

For each edge, the proportion indicates at what percentage of the overall container the edge should be placed\.Zero percent is the container edge, 100 percent is the opposite container edge\.For example, for the top edge, the percentage is counted from the top down\.



<a name="ex_layout_proportional"></a>**A Button centered in, and half the size of its container**


    ^ SpecLayout composed
    	add: #theButton top: 0.25 bottom: 0.25 left: 0.25 right: 0.25;
    	yourself



Also, the argument can be an integer if the offset has to be a fixed number of pixels\.The number of pixels should be positive, as they indicate a distance from the corresponding edge, going to the opposite edge, similar to the arrangement of the proportional layout\.

---

<a name="layout_expert"></a>
The previous examples should cover most of the cases of layout of widgets\.For the remaining cases there is a last way to specify a widget by specifying its position\.

The method `add: origin: corner: ` of `SpecLayout` specifies the layout of a widget, percentage\-wise from the origin point to the corner point\.These two points represent respectively the top left corner and the bottom right corner of the widget\.The arguments express a percentage of the container, so these <u>must</u> be between *0@0* and *1@1* \.

In addition to those points, two offsets can be also be specified, using the method `add: origin: corner:  offsetOrigin: offsetCorner: `\.The offsets specify the number of pixels that the origin and the corner should be moved\.

Contrary to the previous way to define layouts, while using `add: origin: corner:  offsetOrigin: offsetCorner: ` the offset can be negative\.The offset expresses the number of pixels the corresponding corner\.They are expressed in a classical coordinate system with the origin in the top left corner and towards the bottom right corner\.

Note that this approach is similar to the ProportionalLayout of **Morphic**\.


The example [¿?](#ex_layout_expert) shows how to add a widget as a toolbar\.It specifies that the widget in the `toolbar` instance variable should take all the window width, but should be only 30 pixels in height\.



<a name="fig:ex_layout_expert"></a>**Using expert mode to specify a toolbar**


    ^ SpecLayout composed
    	add: #toolbar origin: 0@0 corner: 1@0 offsetOrigin: 0@0 offsetCorner: 0@30;
    	yourself




---

<a name="layout_specify_layout"></a>

As explained in the section [¿?](#subsec_pragma), a UI can have multiple views\.So when a widget layout is specified, the view to use for this sub widget can be specified\.

All the methods seen in the previous examples come with a variant used to specify which view selector to use\.By example, for the `add:` methods there is also  `add:withSpec:`\.

Lets consider a widget **MyWidget** defining a first layout `firstLayout` as the default layout and another one `anotherLayout`\.The example [2\.11](#ex_specify_layout) shows how to add an instance of **MyWidget** using its `anotherLayout` layout\.



<a name="ex_specify_layout"></a>**How to specify an alternative layout**


    ^ SpecLayout composed
    	add: #myWidget withSpec: #anotherLayout;
    	yourself




All the methods can be found in the *commands* and *commands\-advanced* protocols of **SpecLayout**\.

##3\.  Where to find what I want


##4\.  Spec the Dynamic
<a name="sec_spec_the_dynamic"></a>

Having an user interface with a well known number of sub widgets and a static layout is not always sufficient\. A user interface is often more than just that, for example here are two situations where more is needed: First, it happens that the layout of the user interface needs to be changed at runtime to match the execution context of the software\.Second, sub widgets are added or removed at runtime and therefore the programmer need to be able to parametrize those new sub widgets on the fly\.

*Spec* also provides support for such dynamic user interfaces\.In this section we show how to use *Spec* in these situations\.First, we talk about making dynamic modifications of the layout of widgets, and second discuss the dynamic adding and removing of subwidgets\.Third and last we show how the dynamic features can be used to quickly prototype a user interface\.


###4\.1\.  Dynamic modification of the layout


Changing the layout of widgets at runtime is straightforward, as we will see here\.Such changes basically consist of three steps:

1.  creating the new layout,
2.  setting the required flag to prohibit the creation of a new ui element but to reuse the existing one,
3.  building the UI again with the newly created layout\.

&nbsp;

The code in  [4\.1](#rebuildDynamically) is an example of rebuilding a widget with a new layout\.First, a helper method is used to obtain a `SpecLayout` object that determines the new layout\.Second, the `needRebuild` flag is set to `false` to prohibit the creation of a new ui element but to reuse the existing one\.This leads to the replacement of the content of the current container instead of just instantiating a new UI element\.Third, the rebuilding of the user interface is performed\.



<a name="rebuildDynamically"></a>**Rebuild a widget at run time**


    rebuildWithNewLayout
    	| newLayout |
    
    	newLayout := self newLayoutCreatedDynamically.
    	self needRebuild: false. "tells the interpreter to keep my current ui element"
    	self buildWithSpecLayout: newLayout. "rebuilds me with the new layout"



One widget can also keep the UI elements of its sub widgets which did not need to be rebuilt\.The message `needRebuild: false` need to be sent to any of those sub widgets\.If a model composing a *button* and a *list* just want to rearrange the position of the UI elements, there is no need to instantiate new UI elements\.To prevent this, the method `needRebuild:` can be send to them as shown in the example [4\.2](#ex_needRebuild)\.



<a name="ex_needRebuild"></a>**How to need rebuild sub widgets**


    rebuildWithNewLayout
    	| newLayout |
    
    	newLayout := self newLayoutCreatedDynamically.
    	self needRebuild: false.
    	button needRebuild: false.
    	list needRebuild: false.
    	self buildWithSpecLayout: newLayout.




###4\.2\.  Dynamic adding and removal of subwidgets


If an user interface needs a varying number of subwidgets, the amount of which cannot be established at compilation time, then another approach is needed\.In this scenario, `DynamicComposableModel` is the model that needs to be subclassed, as this class provides support for the required kind of dynamic behavior\.

When using `DynamicComposableModel` the instantiation of the sub widgets is a bit different from normal use\.In the `instantiateWidgets` method, instead of instantiating each widget separately, `instantiateModels:` should be used to instantiate them\.This method takes as argument an array of pairs, where each pair is composed of the unique name of the widget as key, and the name of the widget class as value\.This allows for a widget to be accessed by sending a message whose selector is the widget name to the model\.

By example, if a widget named `button` is created, then this widget can be accessed by calling `self button` as shown in the example [4\.3](#ex_dynamic_creation)\.



<a name="ex_dynamic_creation"></a>**Dynamic creation of a widget**


    self instantiateModels: #( button ButtonModel ).
    	self button label: 'Click me'.



Note that the instantiation array can also be an array of pairs\. The previous example could be written



<a name="ex_dynamic_creation2"></a>**Dynamic creation of a widget**


    self instantiateModels: #( button ButtonModel ).
    	self button label: 'Click me'.




###4\.3\.  Example: Prototyping a UI


Thanks to the capability of *Spec* to dynamically instantiate widgets, it is also possible to prototype a user interface from within any workspace\.

The example [4\.5](#ex_prototyping) shows how to easily and quickly design a popup window asking for an input\.



<a name="ex_prototyping"></a>**Popup requiring a simple input**


    view := DynamicComposableModel new
    	instantiateModels: #(label LabelModel text TextInputFieldModel);
    	extent: 300@90;
    	title: 'Choose your project'
    	yourself.
    	
    toolbar := OkToolbar new
    	okAction: [ regex := view text text ];
    	yourself.
    	view focusOrder add: view text.
    view text bindKeyCombination: Character cr asKeyCombination  toAction: [ toolbar triggerOkAction ].
    view label text: 'Packages:'.
    view text
    	autoAccept: true;
    		entryCompletion: nil;
    	ghostText: '.*'.
    view openDialogWithSpecLayout: (SpecLayout composed
    		newRow: [ :r | r add: #label width: 75; add: #text ];
    yourself))
    	toolbar: toolbar;
    	centered;
    	modalRelativeTo: World.



The result can be seen in Figure [4\.1](#fig_popup)\.

<a name="fig\_popup"></a>![fig\_popup](figures/Popup.png "Prototype of a popup")

##5\.  Writing my own basic widget


In the case of a basic widget missing, the following section will explain how to extend the *Spec* framework\.Creating a new widget is essentially three steps: writing a new model, writing an adapter, and updating or creating a specific binding\.

Before explaining the details of how to create a new widget, we will explain how the creation of a widget is done\.It will expose the different actor and provide a clearer understanding of who is doing what\.


###5\.1\.  One step in the building process of a widget

###5\.1\.  Overall view of the build of a widget

The UI building process does not make a distinction between basic and composed widgets\.Hence, at a specific point in the building process of a basic widget the default spec method of the widget is called, just as if it would be a composed widget\.However in this case, instead of providing a layout for multiple widgets that comprise the UI, this method builds an adapter to the underlying UI framework\.Depending of the underlying UI framework that is currently used, this method can provide different kind of adapters, for example an adapter for Morphic, or an adapter for Seaside, etc\.

When a basic widget is built, like the others widget, the model default spec method is called\.But in this case, instead of providing a layout, it builds an adapter\.Depending of the bindings set currently used, it can provide different kind of adapter \(an adapter for Morphic, one for Seaside, etc\.\)\.

This adapter when created will instantiate a UI framework specific widget \(a PluggableListMorph for a MorphicListAdapter\)\.This is this framework specific widget which will be returned by the model and rendered\.

The figure [1\.2\. ](#model_adapter_uielement) shows the relationship between those objects\.

<a name="model\_adapter\_uielement"></a>![model\_adapter\_uielement](figures/Model-Adapter-UIElement.png "Relationship between the model, the adapter, and the ui element")


###5\.2\.  The Model


The new model needs to be a subclass of **AbstractWidgetModel**\.The name of a model is composed of the new basic widget concept \(like list, or button\) and of the word *Model*\.

The model stores all the state of a widget\.



<a name="ex_model_1"></a>**Examples of state**


    index for a list, the label of a button, the action to perform when a text is validated in a text field



The states are stored inside value holders\. They are later used to propagate state changes and thus create the interaction flow\.



<a name="ex_value_holder"></a>**Storing an instance variable as a Value Holder**


    index := 0 asValueHolder.



Then for each state \(in other words instance variable\) three methods should be defined: the getter, the setter, and the registration method\.The first two should be in the protocol *protocol* while the registration method should be in *protocol\-events*\.



<a name="ex_mutators"></a>**Example of mutators for index**


    protocol: protocol
    index
    	index value
    
    protocol: protocol
    index: anInteger
    	index value: anInteger
    
    protocol: protocol-events
    whenIndexChanged: aBlock
    	index whenChangedDo: aBlock



The last step to define a new model is to implement on class side a method `adapterName`\.The method should be in the protocol *spec* and should return a symbol\.The symbol should be composed with the concept of the widget \(like list, or button\) and the word *Adapter* like **ListAdapter**\.

Since a same model can hold the state of different views \(like in the MVC pattern\), multiple adapters can be refering to the same model\.Due to this, the way to update the adapters uses the dependents mechanism\.In fact the message `change: selector with: aCollection` is used to call the message *selector* with the arguments *aCollection* to the adapter\.The propagation is done regardless of the number of adapters\.


###5\.3\.  The Adapter


The adapter name is composed of the framework name \(like Morphic\) and the name of the adapter it is implementing \(like ListAdapter\)\.The adapter is an object used to connect a framework specific ui element and a framework independent model\.The only mandatory method for an adapter is `defaultSpec` on the class side\.

But since the adapter is bridging the gap between the ui element and the model, the adapter often needs to forward the queries form the ui element to the model\.The other way around, since the model is holding the state, the adapter is used to update the ui element state for the model\.

The methods involved in the communication with the model should be in the protocol *spec protocol* while the methods involded in the ui element should be *widget API*\.To communicate with the ui element, the adapter methods use the method `widgetDo:`\.This method execute the block provided as argument only if the ui element has already been created\.


###5\.4\.  The Bindings


The bindings is an object use to resolve the adapter name at run time\.This way a same model can be used with several frameworks\.

Adding the new adapter to the default adapter is quite simple\.It requires to update two methods: `initializeBindings` in **SpecAdapterBindings** and `initializeBindings` in the framework specific adapter class \(like **MorphicAdapterBindings** by example\)\.Once it is done, the bindings should be initialized again with the following snippet:



<a name="snip_bindings"></a>**Reset the bindings**


    SpecInterpreter hardResetBindings

