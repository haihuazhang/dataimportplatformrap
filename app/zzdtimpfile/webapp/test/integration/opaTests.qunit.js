sap.ui.require(
    [
        'sap/fe/test/JourneyRunner',
        'zzdtimpfile/test/integration/FirstJourney',
		'zzdtimpfile/test/integration/pages/FilesList',
		'zzdtimpfile/test/integration/pages/FilesObjectPage'
    ],
    function(JourneyRunner, opaJourney, FilesList, FilesObjectPage) {
        'use strict';
        var JourneyRunner = new JourneyRunner({
            // start index.html in web folder
            launchUrl: sap.ui.require.toUrl('zzdtimpfile') + '/index.html'
        });

       
        JourneyRunner.run(
            {
                pages: { 
					onTheFilesList: FilesList,
					onTheFilesObjectPage: FilesObjectPage
                }
            },
            opaJourney.run
        );
    }
);