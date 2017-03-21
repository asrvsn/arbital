import React, { Component, PropTypes } from 'react'
import { ListItem } from 'material-ui/List';

import AuthorChip from '../AuthorChip'

const styles = {
  entry: {
    display: 'flex',
    flexDirection: 'row',
    alignItems: 'center',
  },
  leftContainer: {
    paddingRight: 10
  },
  rightContainer: {
    display: 'flex',
    justifyContent: 'space-between',
    flexGrow: 1,
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

      <div style={styles.leftContainer}>
        { leftElem || <noscript /> }
      </div>

      <div style={styles.rightContainer}>

        <div style={styles.text}>
          {text}
        </div>

        <div style={styles.chip}>
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
          />
        </div>

      </div>

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
