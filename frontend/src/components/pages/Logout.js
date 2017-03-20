import React, { Component, PropTypes } from 'react'

import Dialog from 'material-ui/Dialog'

export default (props) => {
  return (
    <Dialog
      title="Logged out"
      modal={true}
      open={true}
    />
  )
}
