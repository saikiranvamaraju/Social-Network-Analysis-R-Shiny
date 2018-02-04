# Social-Network-Analysis-R-Shiny
Social Network Analysis on R shiny

Dataset Background and Description: COCAINE DEALING.csv
------------------------------------------------------------------------------------------------------------------------------------------
In the 1990s New York City police investigated a cocaine crime ring by tapping phone conversations between suspected members of a gang. We have in this dataset, 28 individuals who either placed and/or received a call from one of the others in the circle of dealers. The left most column indicates the person who placed the call, and the column names indicate the person who was called. More about this dataset can be found in this paper: \Understanding the structure of a drug tracking organization: a conversational analysis" by M Natarajan. The dataset was downloaded from UCINET.

Social Network Analysis
------------------------------------------------------------------------------------------------------------------------------------------
Some of the questions that were answered using initial part of analysis are:
How many calls were placed in total?
Who received the most calls?
Who placed the most calls?

For each person, degree of centrality ( Indegree, Outdegree & total degree) and betweenness are tabulated along with visualizations.
Graphs for multiple types of networks,i.e.,inbound calls, outbound calls and any type of call were visualized for analysis.

Observations
------------------------------------------------------------------------------------------------------------------------------------------
1. From the inbound and outbound graphs we can infer that, Kay has connection with almost everyone and seems to be the lead for Drug trafficking scandal. 
2. Also, Kay has strong connections with Menna, Dante, Tommy , Steve and Blacky from which we can say these people might be the secondary level leaders taking instructions from Kay.
3. We can see that each of these strong connections are managing a small group of people. For example Dante is seen managing Robert and Parratta works under Menna. 
4. There are also people who take instructions directly from Kay, like charles, Louis etc.,
5. Frank and Fabio had most inbound calls from Kay. They must be the people used by Kay to get majority of his work done.
6. The outbound calls graph suggests that Dante seems to make lot of calls to Kay compared to others. He must be the main source of information for this case.
