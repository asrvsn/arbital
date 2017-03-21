import React, { Component, PropTypes } from 'react'

import Header from '../Header'
import Footer from '../Footer'
import DialogSwitch from '../dialogs/DialogSwitch'

const PageHOC = (ChildComponent) => {
  return (props) => {
    const { isAuthenticated } = props

    return (
      <div>
        <Header {...props} />
        <ChildComponent {...props} />
        <Footer {...props} />
        <DialogSwitch />
      </div>
    )
  }
}

export default PageHOC
