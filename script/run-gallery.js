$(document).ready(function(){
    $('#gallery a').attr('rel', 'gallery').fancybox( {
        beforeShow : function() {
            var alt = this.element.find( 'img' ).attr( 'alt' );
            var title = this.element.find( 'img' ).attr( 'title' );
            this.inner.find( 'img' ).attr( 'alt', alt );
            this.inner.find( 'img' ).attr( 'title', title );
            this.title = title;
        },
        openEffect: 'none',
        closeEffect: 'none',
        helpers : {
            title : {
                type : 'inside'
            }
        }
    } );
});
