# LibreSense
App for sensory analysis

This is  a first attempt  at building  an open source software for sensory analysis.

app-LibreSense actually contains two apps:
the file app.r is  the app for capturing data from  panelists. The idea is that the leader of the sensory panel can modify the name and  number of the samples and also the descriptors. 
A lot of work is required in this app.
One  very important point is that samples  should be randomized or balanced  across the panelists. I imagine that the leader of the panel can upload a spreedsheet with the experimental design  (random or william latin square) and maybe another spreadsheet with the descriptors.
Another important point is that it is not appropriate for the panelist to choose the sample to be tasted because the panelist may make a mistake.  Each sample should appear in sections automatically according to the experimental design.  
An example of automatic sections is in app-Question. The app was made in colaboration with [harpomaxx](https://github.com/harpomaxx) for capturing data from different questions. The questions are grouped by sections and each section advances automatically. Something like this should be done for LibreSense. Each section should be a sample  with the descriptors.
The  second app (server.R and ui.R) is for visualizing the results
. 
