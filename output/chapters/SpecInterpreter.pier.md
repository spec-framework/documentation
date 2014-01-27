

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



###1\.1\.  Collect the data


Before the interpreting loop itself, the interpreter starts by collecting the needed data from the model\.
The first required data is the array to interpret\. Then second required data is the spec wrapper used to encapsulate data during the interpretation\.


The array to interpret is extracted to the layout provided with the model\.
But during the recursive calls of the interpretation loop, the interpreter can be called with any kind of object\.
So the first method is here to extract the data to interpret if any\.
Otherwise it means the recursive calls reached a "primitive object", in other words an object which can not be interpreted\.
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
This argument is the selector of the method to perform to the current receiver \(stored inside the wrapper\)\.
According to the selector, an adequate number of arguments are popped from the array to interpret\.


Each argument will be then interpreted using a new 
**SpecInterpreter** instance and following the exact same process\.
This step is the step leading to recursive calls\.


Then the results of the argument interpretation are provided to the selector while performed over the current receiver\.


The result of this message send is then stored into the current receiver\.


The loop ends when the array is empty\.
The resulting adapter is returned and linked to the model provided to the interpreter\.
