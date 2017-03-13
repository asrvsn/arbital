import React, { Component, PropTypes } from 'react'

const AuthoredListItem = (props) => {
  const { text, authorName, authorId, hrefPath, history } = props
  const authorIcon = <AuthorChip authorName={authorName} authorId={authorId} />

  return (
    <ListItem
      primaryText={text}
      rightIcon={authorIcon}
      onTouchTap={e => history.push(hrefPath)}
    />
  )
}

export default AuthoredListItem
