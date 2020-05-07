function LF_crop(filename) {
        open(filename);
        makeRectangle(0, 0, 1246, 1277);
        run("Crop");
        saveAs("Jpeg", dir2 + filename);
        close();
}

macro "Batch Measure RGB for ROI" { 

        run("Set Measurements...", "area mean standard min integrated limit display redirect=None decimal=3"); 
        dir1 = getDirectory("Choose an Image Directory "); 
        dir2 = getDirectory("Choose an ROI file Directory"); 
        dir3 = getDirectory("Choose an Results Directory");  
        setBatchMode(true); 

		list = getFileList(dir1);
			for (i = 0; i < list.length; i++)
        		LF_crop(dir1, dir2, list[i]);
		
		setBatchMode(false);
   
}