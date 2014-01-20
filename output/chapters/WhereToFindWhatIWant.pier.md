

##1\.  Where to find what I want
<a name="sec_where_to_find_what_I_want"></a>

All the 
*Spec* models for basic widgets have an API that is explicitly documented through the use of pragmas\.
This section explains where to find the API of a model and meaning of the meta information that is attached to the API methods\.


Each model contains at least two protocols that group the public API methods\.
The first protocol is named 
**protocol**\.
It gathers all the methods that set or get the different state elements of the model plus the behavioural methods acting directly on these elements\.
The second protocol is named 
**protocol\-events**\.
It gathers all the methods that are used to register to a state change\.


All the meta\-information of public API methods is documented through the use of pragmas that start with 
*api:*\.
There are three types of public API methods: getters, setters and registration methods\.



###1\.1\.  Meta information for getters


The pragma for getters is always 
`<api: #inspect>`\.
For example, the code in 
[1\.1](#ex_api_getter) shows how the 
*action*  method in 
**ButtonModel** is implemented\.




<a name="ex_api_getter"></a>**Implementation of ButtonModel>>\#action**


    action
    	<api: #inspect>
    	"get the block performed when the button is clicked"
    
    	^ actionHolder value




###1\.2\.  Meta information for setters


The pragma for setters is a bit more complex\.
The pattern of the pragma is 
`<api: typeOfState getter: getterSelector registration: registrationMethodSelector>`\.
In this pattern, 
*typeOfState* , 
*getterSelector* and 
*registrationMethodSelector* need to be substituted by the relevant values for this setter\.


The 
*getterSelector* specifies the name of the getter method \(a Symbol\) that returns the state set by this method\.
The 
*registrationMethodSelector* states the name of the method \(a Symbol\) that needs to be used to register to changes in the state\.
*typeOfState*  gives the type of the state being set by this setter\.
The possible types are as follows:



-  \#block indicates a block, 
-  \#boolean indicates a boolean,
-  \#color indicates a Color,
-  \#image indicates a Form,
-  \#integer indicates an integer,
-  \#point indicates a Point,
-  \#string indicates a String,
-  \#st indicates any other type of *Smalltalk* object\.


For example, the code in 
[1\.2](#ex_api_setter) shows how 
*actions:* is implemented in 
**ButtonModel**\.




<a name="ex_api_setter"></a>**Implementation of ButtonModel>>\#action:**


    action: aBlock
    	<api: #block getter: #getAction registration: #whenActionChangedDo:>
    	"set the block performed when the button is clicked"
    
    	actionHolder value: aBlock




###1\.3\.  Meta information for registration methods


The pragma for registration methods information always is 
*<api: \#event>*\.
For example, the code in 
[1\.3](#ex_api_registration) shows how the method 
*whenActionChangedDo:* is implemented in 
**ButtonModel**\.




<a name="ex_api_registration"></a>**Implementation of ButtonModel>>\#whenActionChangedDo**


    whenActionChangedDo: aBlock 
    	<api: #event>
    	"Set the block performed when the action to perform is changed"
    
    	actionHolder whenChangedDo: aBlock




###1\.4\.  Meta information for behavioural method


The other methods should be mainly behavioural methods\.
The pragma for these methods is 
*<api: \#do>*\.
The example 
[1\.4](#ex_resetSelection) shows how 
*resetSelection* is implemented in 
**ListModel**\.




<a name="ex_resetSelection"></a>**Implementation of ListModel>>\#resetSelection**


    resetSelection
    	<api: #do>
    	"Unselect every items"
    
    	selectionHolder reset.
    	multiSelectionHolder removeAll

