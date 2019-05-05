const server = "http://localhost:9000"

var removeHighlighting = function() {
  $('#board .square-55d63')
    .removeClass("hl")
    .removeClass("hl-origin");
};

var hl = function(square) {
  const element = $('#board .square-' + square);
  element.addClass("hl");
};

var hlOrigin  = function(square) {
  const element = $('#board .square-' + square);
  element.addClass("hl");
  element.addClass("hl-origin");
};

var serverFailure = function(e) {
  $('#server-status').css("display", "block")
  $('#server-status p').html("Server fault. " + e)
};

let x;

var getMoves = function(square) {
  $.ajax({
    url: server + '/game/1/moves?pos=' + square,
    crossDomain: true,
    succes: (response) => console.log(response),
    error: (xhr, status) =>
      {
        console.log(xhr, status);
        serverFailure(status);
      }
  });

  return [];
};

var onclickSquare = function(square) {
  removeHighlighting();
  const moves = getMoves(square);

  if (moves.length === null) return;

  //highligth the selected piece
  hlOrigin(square);


  moves.forEach((x) => {
    hl(x);
  });
};

$(document).on('click', '#board .square-55d63 .piece-417db', (event) => {
  const squareElement = $(event.target.parentElement);
  const alreadyHighlighted = squareElement.data('on');

  if (alreadyHighlighted === true) {
    removeHighlighting();
    squareElement.data("on", false);
    return;
  }

  const square = squareElement.context.id.substring(0,2);
  squareElement.data("on", true);
  onclickSquare(square);
});

let board; //TODO remove, but mighty handy to have in global scope.

$(document).ready(() => {
  board = ChessBoard('board', 'start');
});

