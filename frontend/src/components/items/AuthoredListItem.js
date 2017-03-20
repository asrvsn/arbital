import React, { Component, PropTypes } from 'react'
import { ListItem } from 'material-ui/List';

import AuthorChip from '../AuthorChip'

const styles = {
  entry: {
    display: 'flex',
    flexDirection: 'row',
    justifyContent: 'space-between',
    alignItems: 'center'
  },
  text: {
  },
  chip: {
  },
}

const AuthoredListItem = (props) => {
  const {
    text,
    authorName,
    authorId,
    onTouchTap,
    onAuthorTouchTap,
    leftElem,
    hoverElem
  } = props

  const entry = (
    <div style={styles.entry}>

      { leftElem || <noscript /> }

      <div style={styles.text}>
        {text}
      </div>

      <AuthorChip
        authorName={authorName}
        authorId={authorId}
        onTouchTap={e => {
          e.preventDefault()
          e.stopPropagation()
          if (!! onAuthorTouchTap) {
            onAuthorTouchTap(e)
          }
        }}
        style={styles.chip}
      />

    </div>
  )

  return (
    <ListItem
      primaryText={entry}
      onTouchTap={e => {
        if (onTouchTap) {
          onTouchTap(e)
        }
      }}
    >
    </ListItem>
  )
}

export default AuthoredListItem
