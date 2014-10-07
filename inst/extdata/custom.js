(function(chm){
function _chm_as(src){var s=document.createElement('script');
 s.setAttribute('type','text/javascript'); s.setAttribute('src',src);
 $('head').append(s);
}
_chm_as ("http://bioinformatics.mdanderson.org/chmv/js/libs/dynamic_viewer.v2.js");
chm.addCustomization(function(){
    var dvapp = new DynamicViewer.DVApp ("/DynamicViewer", "dynjs", "getchmcustom");
    var params = { 'chm': chm.mapName };
    var target_id = "noonewantsme";
    var target = $('<div></div>', { id: target_id, class: 'offscreen' });
    $('body').append(target);
    DynamicViewer.invoke (dvapp, params, target_id, function (error, result) {
	if (!error) {
            _chm_as (result);
	}
    });
});
})(MDACC_GLOBAL_NAMESPACE.namespace('tcga').chm);
