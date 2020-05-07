// function to select wing, rotate (using ferets angle) and then reselect rotated wing
function rotate_wing_feret_reselect(filepath){
	
	// select wing using threshold method
	nameImg=getTitle();
	selectImage(nameImg);
	run("Duplicate...", "title=greylev");
	run("8-bit");
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
	run("Clear Outside");

	// get measurements of image
	List.setMeasurements; // alternative to measure-getResult, no table involved
	dx = round(getWidth()/2 - parseInt(List.get("X")));
	dy = round(getHeight()/2 - parseInt(List.get("Y")));
	roi_x = parseInt(List.get("BX"))+dx;
	roi_y = parseInt(List.get("BY"))+dy;
	feret_angle = parseInt(List.get("FeretAngle"));

	// translate object & ROI to center of image (so no clippin occurs when rotated)
	run("Select All");
	run("Cut");
	
	run("Restore Selection"); //juggling the original roi, in other contexts can be called from the ROI manager
	run("Select None");
	
	run("Select All");
	Roi.move(dx, dy);
	run("Paste");
	
	run("Restore Selection"); //juggling again
	run("Select None");

	// now rotate object and then ROI using ferets angle
	run("Rotate...", "angle=&feret_angle interpolation=Bilinear fill");
	run("Restore Selection");
	Roi.move(roi_x, roi_y);
	run("Rotate...", "rotate angle=&feret_angle");
	// add new ROI to ROI manager
	roiManager("Add");
    roiManager("Select", 0);
	roiManager("Delete");
   	run("8-bit");
}



// measure mean gey value of ROI in thirds
// creates overall 4 ROIs (full wing and 3 sections of the wing)

function ROI_to_thirds() { 
	// set new measurements
	run("Set Measurements...", "area mean standard min fit feret's integrated display redirect=None decimal=2");
	  // select the ROI of wing
	  roiManager("Select", 0);
	  // get the mean grey value of the three sections of the wing
	  firstROI = roiManager("count")-1;
	  getSelectionBounds(x, y, width, height);
	  third = width / 3;
		  for (i=0; i<3; i++) {
		    makeRectangle (x+(i*third), y, third, height);
		    roiManager("Add");
		    roiManager("select", roiManager("count")-1);
		    roiManager("rename", "rectangle");
		    roiManager("select", newArray(firstROI, roiManager("count")-1));
		    roiManager("and");
		    roiManager("Add");
		    roiManager("deselect");
		    roiManager("select", roiManager("count")-2);
		    roiManager("delete");
		  	}
	 run("8-bit"); //next part selects each section of the ROI - renames them and takes a measurment
	 roiManager("Select", 1);
	 roiManager("Rename", "edge3rd");
	 roiManager("Measure");
	 roiManager("Select", 2);
	 roiManager("Rename", "mid3rd");
	 roiManager("Measure");
	 roiManager("Select", 3);
	 roiManager("Rename", "bot3rd");
	 roiManager("Measure");
	 roiManager("Select", 0);
	 roiManager("Rename", "whole_wing");
	 roiManager("Measure");
	 roiManager("Delete");
	 roiManager("Deselect");
	 roiManager("Delete");
	 IJ.renameResults("Results");
}

// run functions above in macro across multiple sub-directories

macro "measure_wing_thirds" { 
   requires("1.33s"); 
   dir = getDirectory("Choose a Directory with folders of pics"); // this is a folder containing sub-folders with .png pictures in
   dir2 = getDirectory("Choose an Results Directory"); // this is the directory where the results will be output
   setBatchMode(true);
   count = 0;
   countFiles(dir);
   n = 0;
   processFiles(dir);
   //print(count+" files processed");
   
   function countFiles(dir) {
      list = getFileList(dir);
      for (i=0; i<list.length; i++) {
          if (endsWith(list[i], "/"))
              countFiles(""+dir+list[i]);
          else
              count++;
      }
  }

   function processFiles(dir) {
      list = getFileList(dir);
      for (i=0; i<list.length; i++) {
          if (endsWith(list[i], "/"))
              processFiles(""+dir+list[i]);
          else {
             showProgress(n++, count);
             path = dir+list[i];
             processFile(path);
          }
      }
  }

  function processFile(path) {
       if (endsWith(path, ".png")) {
           open(path);
           rotate_wing_feret_reselect(path);
           ROI_to_thirds();
           path2 = dir2; 
			IJ.renameResults("Results");
			saveAs("Results",path2+"HW_V_mGV_per_3rd.csv"); 
			}
  }
}