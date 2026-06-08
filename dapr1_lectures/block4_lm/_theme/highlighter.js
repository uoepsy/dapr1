<script src="https://code.jquery.com/jquery-3.6.0.min.js"></script>
<script>
$(document).ready(function() {
    $('.cell').each(function() {
        let highlightLines = $(this).attr('data-output-line-numbers');
        if (highlightLines) {
            let lines = highlightLines.split(',').map(x => parseInt(x));
            $(this).find('.cell-output-stdout pre code').html(function(_, html) {
                return html.split('\n').map((line, index) => {
                    if (lines.includes(index + 1)) {
                        return '<span style="background-color: #D0D8FC;">' + line + '</span>';
                    } else {
                        return line;
                    }
                }).join('\n');
            });
        }
    });
});
</script>
