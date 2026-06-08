<script>
  // Grab elements
  const overlayIframe = document.getElementById('overlayIframe');
  const overlayClose  = document.getElementById('overlayClose');

  // Show overlay
  function showOverlay(src) {
    overlayIframe.src = src;
    overlayIframe.classList.add('visible');
    overlayClose.classList.add('visible');
  }

  // Hide overlay
  function hideOverlay() {
    overlayIframe.classList.remove('visible');
    overlayClose.classList.remove('visible');
    setTimeout(() => {
      if (!overlayIframe.classList.contains('visible')) {
        overlayIframe.src = 'about:blank';
      }
    }, 300);
  }

  // Trigger overlay on click
  document.addEventListener('click', function (e) {
    if (e.target.classList.contains('iframe-trigger')) {
      const src = e.target.getAttribute('data-src');
      if (src) showOverlay(src);
    }
  });

  // Close button
  overlayClose.addEventListener('click', hideOverlay);

  // Key handling
  document.addEventListener('keydown', function (e) {
    if (overlayIframe.classList.contains('visible')) {
      if (e.key === 'Escape') {
        // Only intercept ESC when overlay is open
        e.preventDefault();
        hideOverlay();
      } else if (["ArrowLeft","ArrowRight","ArrowUp","ArrowDown"," "].includes(e.key)) {
        // Block navigation keys *only* while overlay is visible
        e.preventDefault();
      }
      // Do NOT stopPropagation — let Reveal update UI (slide numbers etc.)
    }
  });
</script>
