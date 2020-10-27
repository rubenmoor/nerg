    main :: Effect Unit
        main = do
      w <- window
      loop w

    loop :: Window -> Effect Unit
    loop w = redraw <* requestAnimationFrame (loop w) w

    redraw :: Effect Unit
    redraw = log "Redrawing endlessly!"

    loop :: Window -> Effect Unit
    loop = do
      redraw
      requestAnimationFrame (loop w) w $> unit
