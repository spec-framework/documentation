

##1\.  The Spec interpreter
<a name="sec_spec_interpreter"></a>

In order to create a framework specific UI element a 
*Spec* model is interpreted via the 
**SpecInterpreter**\.
The interpreter is in charge to recursively interpret all the sub widgets of a model\.
It then combine then according to the given layout and the given binding\.


The interpreter entry point is the method 
`SpecInterpreter>>#interpretASpec:selector:`\.


The section will explain the different part of a model interpretation\.
The flow diagram 
[1\.1](#fig_flow_diagram) shows a different steps of the interpretation of a model\.


<a name="fig_flow_diagram"></a>![fig_flow_diagram](figures/Interpretation_Chart.png "Spec interpretation flow chart")


###1\.1\.  Collect the data


Before the interpreting loop itself, the interpreter starts by collecting the needed data from the model\.
The first required data is the array to interpret\. Then second required data is the spec wrapper used to encapsulate data during the interpretation\.


The array to interpret is extracted to the layout provided with the model\.
The code 
[1\.1](#ex_extract_array) shows the convertion of a SpecLayout into an array of literals that the interpreter can iterate over\.




<a name="ex_extract_array"></a>**Convertion of a SpecLayout into an Array of literals**


    SpecLayout composed
    	newRow: [ :row | row add: #theList ];
    	asArray
    	 
    "returns"	 
    
    #(#ContainerModel 
    	#add: #(
    		#(#ContainerModel 
    			#add: #(
    				#(#model #theList) 
    					#layout: #(
    						#SpecLayoutFrame 
    							#leftFraction: 0 
    							#topFraction: 0 
    							#rightFraction: 1 
    							#bottomFraction: 1 
    							#leftOffset: 0 
    							#topOffset: 0 
    							#rightOffset: 0 
    							#bottomOffset: 0
    					)
    			)
    		) #layout: #(
    			#SpecLayoutFrame 
    				#leftFraction: 0 
    				#topFraction: 0 
    				#rightFraction: 1 
    				#bottomFraction: 1 
    				#leftOffset: 0 
    				#topOffset: 0 
    				#rightOffset: 0 
    				#bottomOffset: 0
    		)
    	)
    )



But during the recursive calls of the interpretation loop, the interpreter can be called with any kind of object\.
So the first method is here to extract the data to interpret if any\.
Otherwise it means the recursive calls reached a "primitive object" \(like interger in the example 
[1\.1](#ex_extract_array)\), in other words an object which can not be interpreted\.
In this case the primitive object is directly returned\.


The second data to collect if the first step succeeded is the spec wrapper to use along the interpretation\.
This step checks if the model provided as argument needs to be rebuild or not\.
If not, the model current UI element is directly returned\.
Otherwise this step results in the creation of a wrapper object keeping the current model as well as the receiver of messages to perform\.
The wrapper type is based on the array first element\.
The wrapper will at the end provide the result of the interpretation\.



###1\.2\.  Interpretation loop


Once all the required data are collected, the interpretation loop can begin\.


The loop is quite simple\. The first element of the array is popped out of the array to interpret\.
This literal is the selector of the method to perform on the current receiver \(stored inside the wrapper\)\.
According to the selector, an adequate number of arguments are popped from the array to interpret\.


Each argument will be then interpreted using a new 
**SpecInterpreter** instance and following the exact same process\.
This step is the step leading to recursive calls\.


Then the results of the argument interpretation are provided to the selector while performed over the current receiver\.


The result of this message send is then stored into the current receiver\.


The loop ends when the array is empty\.
The resulting adapter is returned and linked to the model provided to the interpreter\.
