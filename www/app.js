// Collapse/expand functionality for plot annotations
$(document).on('shown.bs.collapse', '.collapse', function() {
  var icon = $(this).prev().find('i');
  icon.removeClass('fa-caret-right').addClass('fa-caret-down');
}).on('hidden.bs.collapse', '.collapse', function() {
  var icon = $(this).prev().find('i');
  icon.removeClass('fa-caret-down').addClass('fa-caret-right');
});

// Enhanced clipboard functionality with better error handling
Shiny.addCustomMessageHandler('copyToClipboard', function(text) {
  console.log('Attempting to copy to clipboard:', text);
  
  if (navigator.clipboard && window.isSecureContext) {
    // Use modern clipboard API if available
    navigator.clipboard.writeText(text).then(function() {
      console.log('✅ Text copied to clipboard successfully using modern API');
    }).catch(function(err) {
      console.error('❌ Modern clipboard API failed:', err);
      // Fallback to older method
      fallbackCopyTextToClipboard(text);
    });
  } else {
    console.log('⚠️ Modern clipboard API not available, using fallback');
    // Fallback for older browsers or non-secure contexts
    fallbackCopyTextToClipboard(text);
  }
});

function fallbackCopyTextToClipboard(text) {
  console.log('Using fallback clipboard method');
  var textArea = document.createElement('textarea');
  textArea.value = text;
  textArea.style.top = '0';
  textArea.style.left = '0';
  textArea.style.position = 'fixed';
  textArea.style.width = '2em';
  textArea.style.height = '2em';
  textArea.style.padding = '0';
  textArea.style.border = 'none';
  textArea.style.outline = 'none';
  textArea.style.boxShadow = 'none';
  textArea.style.background = 'transparent';
  
  document.body.appendChild(textArea);
  textArea.focus();
  textArea.select();
  
  try {
    var successful = document.execCommand('copy');
    if (successful) {
      console.log('✅ Fallback: Copying text command was successful');
    } else {
      console.log('❌ Fallback: Copying text command was unsuccessful');
      alert('Copy failed. Please manually copy the URL from the browser address bar.');
    }
  } catch (err) {
    console.error('❌ Fallback: Oops, unable to copy', err);
    alert('Copy failed. Please manually copy the URL from the browser address bar.');
  }
  
  document.body.removeChild(textArea);
}