(function(chm){
    $.getScript ("/resources/dyce.js", function() {
	chm.addCustomization(function(){
	    var dvapp = new DynamicViewer.DVApp ("/DynamicViewer", "dynjs", "getchmcustom");
	    var params = { 'chm': chm.mapName };
	    var target_id = "noonewantsme";
	    var target = $('<div></div>', { id: target_id, class: 'offscreen' });
	    $('body').append(target);
	    DynamicViewer.invoke (dvapp, params, target_id, function (error, result) {
		$.getScript (error ? chm.urlBase + "/undefined/custom-backup.js" : result);
	    });
	});
    });
})(MDACC_GLOBAL_NAMESPACE.namespace('tcga').chm);
