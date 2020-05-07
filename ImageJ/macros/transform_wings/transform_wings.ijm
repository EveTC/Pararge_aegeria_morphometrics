function transform(input, output, filename) {
        open(input + filename);
        run("Flip Horizontally");
        saveAs("Jpeg", output + filename);
        close();
}
	
input = "C:/Users/etayl/Dropbox/PhD/Morphometrics/PA_phenotypic_variation/photos/GBS pops/cropped/Dorsal/RF_to_transform/";
output = "C:/Users/etayl/Dropbox/PhD/Morphometrics/PA_phenotypic_variation/photos/GBS pops/cropped/Dorsal/RF_transformed/";

setBatchMode(true); 
list = getFileList(input);
for (i = 0; i < list.length; i++)
        transform(input, output, list[i]);
setBatchMode(false);