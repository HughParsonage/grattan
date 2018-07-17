// Start Javascript and JQuery Calls
// Pass the jQuery object as $
jQuery(function($) {	

// Search box clear and set
$(document).ready(function(){
	$('#edit-search-block-form--2').attr('value', 'Search');
});	
$('#edit-search-block-form--2').focus(function () {
	if ($('#edit-search-block-form--2').attr('value') == 'Search')  {	
		$(this).attr('value', '');		
	}
});	

// Mobile Navigation show/hide
$('#_guidenav').click(function(){	
	if($('#global .menu').hasClass('mob')) {		
		$('#global .menu').slideUp(800, 'easeOutExpo', function(){$(this).removeClass('mob')});
	} 
	else {	
		$('#global .menu').addClass('mob').slideDown(1000,'easeOutExpo');
	}
});
$('#_navigation').click(function(){	
	if($('#navigation .menu-block > .menu').hasClass('mob')) {		
		$('#navigation .menu-block > .menu').slideUp(800, 'easeOutExpo', function(){$(this).removeClass('mob')});
		$('#navigation .menu ul').removeClass('mob-sub-menu');		
	} 
	else {	
		$('#navigation .menu ul').addClass('mob-sub-menu');
		$('#navigation .menu-block > .menu').addClass('mob').slideDown(1000,'easeOutExpo');
	}
});

// Zebra stripes
$(document).ready(function(){
	$('table tr:odd').addClass('zebra');
});


// A-to-Z indexes show & hides
$(document).ready(function(){
	$('#view-keyword-filter > ul').hide(); // Hide the children	
	$('#view-keyword-filter > h3').each(function() { // Add on a show/Hide link for the expander
		if ($(this).next('ul').length > 0) {			
			$(this).prepend('<a href="javascript:void(0)" class="az-show-hide"><span class="offleft">Show/hide children</span></a> ');
		}	
	});
});

// A-to-Z indexes Show and Hide the next ul on each click
$('.az-show-hide').click(function(){	
	if($(this).hasClass('expanded')) {
		$(this).removeClass('expanded');
		$(this).parent('h3').next('ul').slideUp(1000,'easeOutExpo');	
	}
	else {
		$(this).addClass('expanded');
		$(this).parent('h3').next('ul').slideDown(1000,'easeOutExpo');	
	}
});


// Sitemap show & hides
$(document).ready(function(){
	$('#content .menu-block > ul ul').hide();	
	$('#content .menu-block li').each(function() { // Add on a show/Hide link for the expander
		if ($(this).children('ul').length > 0) {			
			$(this).prepend('<a href="javascript:void(0)" class="show-hide"><span class="offleft">Show/hide children</span></a> ');
		}		
	});
});

// Act Reference indexes show & hides
$(document).ready(function(){
	$('#view-act-references > ul ul').hide(); // Hide the children	
	$('#view-act-references > ul > li').each(function() { // Add on a show/Hide link for the expander
		if ($(this).children('ul').length > 0) {			
			$(this).prepend('<a href="javascript:void(0)" class="show-hide"><span class="offleft">Show/hide children</span></a> ');
		}	
	});
});

// Act Ref & Sitemap, Show and Hide the children on each click
$('.show-hide').click(function(){	
	if($(this).hasClass('expanded')) {
		$(this).removeClass('expanded');
		$(this).parent('li').children('ul').slideUp(1000,'easeOutExpo');	
	}
	else {
		$(this).addClass('expanded');
		$(this).parent('li').children('ul').slideDown(1000,'easeOutExpo');	
	}
});

// End jQuery	
});;
