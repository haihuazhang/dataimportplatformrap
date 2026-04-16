sap.ui.require(
    [
        'sap/fe/test/JourneyRunner',
        'zzdtimpconf/test/integration/FirstJourney',
		'zzdtimpconf/test/integration/pages/ConfigurationList',
		'zzdtimpconf/test/integration/pages/ConfigurationObjectPage'
    ],
    function(JourneyRunner, opaJourney, ConfigurationList, ConfigurationObjectPage) {
        'use strict';
        var JourneyRunner = new JourneyRunner({
            // start index.html in web folder
            launchUrl: sap.ui.require.toUrl('zzdtimpconf') + '/index.html'
        });

       
        JourneyRunner.run(
            {
                pages: { 
					onTheConfigurationList: ConfigurationList,
					onTheConfigurationObjectPage: ConfigurationObjectPage
                }
            },
            opaJourney.run
        );
    }
);