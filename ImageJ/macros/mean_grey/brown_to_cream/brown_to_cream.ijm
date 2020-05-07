// macro to measure the brown and cream areas seperately to get an idea of contrast

// set measurements
run("Set Measurements...", "area mean standard min center fit feret's integrated display redirect=None decimal=2");

// select whole wing
nameImg=getTitle();
selectImage(nameImg);
run("Duplicate...", "title=greylev");
run("8-bit");
//setAutoThreshold("Mean");
run("Auto Threshold", "method=Huang");
run("Convert to Mask");
run("Fill Holes");
run("Erode");
run("Create Selection");
selectImage(nameImg);
run("Restore Selection");
run("ROI Manager...");
roiManager("Add");
close("greylev");
run("Clear Outside");
run("Select None");

// select cream patches

run("Duplicate...", "title=greylev");
run("8-bit");
//setAutoThreshold("Mean");
run("Auto Threshold", "method=Intermodes");
run("Convert to Mask");
run("Fill Holes");
run("Erode");
run("Create Selection");
selectImage(nameImg);
run("Restore Selection");
run("ROI Manager...");
roiManager("Add");
close("greylev");

// change to 8-bit
run("8-bit");

// re-name ROIs
roiManager("Select", 1);
roiManager("Rename", "cream");
roiManager("Select", 0);
roiManager("Rename", "wing");

// make seperate whole wing one which will be used to measure once cream has been removed
roiManager("Add");
roiManager("Select", 2);
roiManager("Rename", "brown");

// measure whole wing
roiManager("Select", 0);
run("Measure");

// measure cream patches
roiManager("Select", 1);
run("Measure");

// remove cream areas from wing
run("Clear", "slice");

// measure brown area - reselect whole wing and rename to brown only
roiManager("Select", 2);
run("Measure");
