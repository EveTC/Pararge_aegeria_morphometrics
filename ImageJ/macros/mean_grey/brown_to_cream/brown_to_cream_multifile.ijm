// Measure: 1) full wing; 2) brown area; 3) cream area across multiple files

function brown_vs_cream(filepath){
		// macro to measure the brown and cream areas seperately to get an idea of contrast

		// set measurements
		run("Set Measurements...", "area mean standard min center fit feret's integrated display redirect=None decimal=2");
		
		// select whole wing
		nameImg=getTitle();
		selectImage(nameImg);
		run("Duplicate...", "title=greylev");
		run("8-bit");
		//setAutoThreshold("Mean");
		run("Auto Threshold", "method=Otsu");
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
		run("Auto Threshold", "method=MaxEntropy");
		run("Convert to Mask");
		run("Fill Holes");
		run("Erode");
		run("Create Selection");
		selectImage(nameImg);
		run("Restore Selection");
		run("ROI Manager...");
		roiManager("Add");
		close("greylev");
		
		// rename ROIs
		roiManager("Select", 0);
		roiManager("Rename", "wing");
		roiManager("Select", 0);
		run("Measure");
		roiManager("Select", 1);
		roiManager("Rename", "cream");
		roiManager("Select", 1);
		run("Measure");
		
		// make combination of whole wing and cream to get brown area
		roiManager("Select", 0); // wing ROI
		run("Make Inverse"); // look at 
		roiManager("Add");
		
		// rename inverse wing ROI
		roiManager("Select", 2);
		roiManager("Rename", "wing_inv");
		
		// select wing_inv and cream ROI to combine
		roiManager("Select", newArray(1,2)); // select inverse wing ROI and cream ROI
		roiManager("Combine"); // combine these ROIs - overwrites cream
		
		// rename com_inverse ROI
		roiManager("Select", 1);
		roiManager("Rename", "combin_inv");
		
		// select comb_inv ROI and revert back to normal
		roiManager("Select", 1);
		run("Make Inverse"); // revet ROI
		roiManager("Add");
		
		// rename combine ROI
		roiManager("Select", 3);
		roiManager("Rename", "brown");
		run("Clear Outside"); // clear cream patches
		
		// change to 8-bit
		run("Select None");
		run("8-bit");
		
		// measure brown area w/o cream area
		roiManager("Select", 3);
		run("Measure");

		// delete ROIs
		roiManager("Delete");
		roiManager("Deselect");
		roiManager("Delete");
}

// run functions above in macro across multiple sub-directories

macro "measure_brown_vs_cream" { 
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
           brown_vs_cream(path);
           path2 = dir2; 
			IJ.renameResults("Results");
			saveAs("Results",path2+"FW_D_bwnVscrm.csv"); 
			}
  }
}