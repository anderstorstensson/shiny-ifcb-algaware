// Gallery interaction JavaScript for algaware
// Adapted from ClassiPyR's gallery.js

$(document).ready(function() {
  var wasDragging = false;
  var startX, startY;
  var selectionBox = null;

  // Single-click selection on image cards
  $(document).on('click', '.image-card', function(e) {
    if (wasDragging) {
      wasDragging = false;
      return;
    }
    var imgId = $(this).data('img');
    Shiny.setInputValue('gallery-toggle_image',
      {img: imgId, time: new Date().getTime()},
      {priority: 'event'});
  });

  // Drag-select
  $(document).on('mousedown', '.gallery-drag-area', function(e) {
    if ($(e.target).closest('.image-card').length > 0 &&
        !e.shiftKey) return;

    startX = e.clientX;
    startY = e.clientY;
    wasDragging = false;

    selectionBox = $('<div class="selection-box-active"></div>');
    selectionBox.css({
      position: 'fixed',
      border: '2px dashed #007bff',
      background: 'rgba(0,123,255,0.1)',
      'pointer-events': 'none',
      'z-index': 1000,
      left: startX + 'px',
      top: startY + 'px',
      width: '0px',
      height: '0px'
    });
    $('body').append(selectionBox);

    e.preventDefault();
  });

  $(document).on('mousemove', function(e) {
    if (!selectionBox) return;

    var dx = Math.abs(e.clientX - startX);
    var dy = Math.abs(e.clientY - startY);
    if (dx > 5 || dy > 5) wasDragging = true;

    selectionBox.css({
      left: Math.min(e.clientX, startX) + 'px',
      top: Math.min(e.clientY, startY) + 'px',
      width: Math.abs(e.clientX - startX) + 'px',
      height: Math.abs(e.clientY - startY) + 'px'
    });
  });

  $(document).on('mouseup', function(e) {
    if (!selectionBox) return;

    if (wasDragging) {
      var boxRect = {
        left: Math.min(e.clientX, startX),
        top: Math.min(e.clientY, startY),
        right: Math.max(e.clientX, startX),
        bottom: Math.max(e.clientY, startY)
      };

      var selected = [];
      $('.image-card').each(function() {
        var rect = this.getBoundingClientRect();
        if (rect.left < boxRect.right && rect.right > boxRect.left &&
            rect.top < boxRect.bottom && rect.bottom > boxRect.top) {
          selected.push($(this).data('img'));
          $(this).addClass('selected');
        }
      });

      if (selected.length > 0) {
        Shiny.setInputValue('gallery-drag_select',
          {images: selected, time: new Date().getTime()},
          {priority: 'event'});
      }
    }

    selectionBox.remove();
    selectionBox = null;
  });
});
