import React, { Component, PropTypes } from 'react'

const AuthoredListItem = (props) => {
  const { text, authorName, authorId } = props
  const authorIcon = <AuthorChip authorName={authorName} authorId={authorId} />

  return (
    <ListItem primaryText={text} rightIcon={authorIcon} />
  )
}

export default AuthoredListItem
